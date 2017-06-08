#ifndef LLVM_ELF_LOADER_HPP
#define LLVM_ELF_LOADER_HPP

// Clarification-relocation.
//
// Elf loader provide information about common entries like segments, sectuions and symbols
// Also it provides information about relocations, and there are some details here.
//
// Relocation info is targeting mainly for relocatable files like shared libraries or kernel
// modules. Such files don't have entry point or segments, symbol addresses etc. and contain
// calls to unresolved locations like in example below.
//
// ...
// 0000000000000014 <my_fun>:
//    14:	55                   	push   %rbp
//    15:	48 89 e5             	mov    %rsp,%rbp
//    18:	48 83 ec 18          	sub    $0x18,%rsp
//    1c:	89 7d ec             	mov    %edi,-0x14(%rbp)
//    1f:	c7 45 f8 2a 00 00 00 	movl   $0x2a,-0x8(%rbp)
//    26:	8b 55 ec             	mov    -0x14(%rbp),%edx
//    29:	8b 45 f8             	mov    -0x8(%rbp),%eax
//    2c:	89 d6                	mov    %edx,%esi
//    2e:	89 c7                	mov    %eax,%edi
//--> 30:	e8 00 00 00 00       	callq  35 <my_fun+0x21>
//    35:	89 45 fc             	mov    %eax,-0x4(%rbp)
//    38:	8b 45 fc             	mov    -0x4(%rbp),%eax
//    3b:	c9                   	leaveq
//    3c:	c3                   	retq
// ...
//
// 0x31 is offset where some changes in address expected - 00 00 00 00 defenetly is not
// an address. It could be a reference to a symbol defined in same file or to a symbol defined somewhere
// else (external symbol).
// So, our task is to resolve this case, i.e. to find a mapping from this offset to something sensible.
//
// First of all we should use absoulute offset, i.e. file offsets to make every mapping unique.
// So full offset in example above will be computed as section offset + 0x31. And it is a place
// where relocation should be applied.
//
// We define two attributes for relocations, and every relocation is represented only by one of them:
// 1) symbol-reference that is a mapping from file offset to a symbol at some file offset and
//    with some meaningful size;
// 2) external-symbol that is a mapping from file offset to some name.
//
// TODO: find where abs symbols go (__crc_usbatm_usb_disconnect)


#include <iostream>
#include <iomanip>

#include <llvm/Object/ELFObjectFile.h>

// TODO: remove it, we don't need in it - just for debug
#include <llvm/Object/RelocVisitor.h>


#include "llvm_error_or.hpp"
#include "llvm_loader_utils.hpp"

namespace loader {
namespace elf_loader {

using namespace llvm;
using namespace llvm::object;

static const std::string elf_declarations =
    "(declare file-type (name str))"
    "(declare arch (name str))"
    "(declare entry-point (addr int))"
    "(declare relocatable (flag bool))"
    "(declare program-header (name str) (offset int) (size int))"
    "(declare virtual-program-header (name str) (addr int) (size int))"
    "(declare program-header-flags (name str) (load bool) (read bool) (write bool) (execute bool))"
    "(declare section-header (name str) (addr int) (size int) (offset int))"
    "(declare section-flags (name str) (write bool) (execute bool))"
    "(declare symbol-entry (name str) (addr int) (size int))"
    "(declare code-entry (addr int) (name str))"
    "(declare symbol-reference (offset int) (addr int) (size int))"
    "(declare external-symbol (addr int) (name str))";

template <typename T>
bool is_rel(const ELFObjectFile<T> &obj) {
    auto hdr = obj.getELFFile()->getHeader();
    return (hdr->e_type & ELF::ET_REL);
}

template <typename T>
void file_header(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto hdr = obj.getELFFile()->getHeader();
    s.entry("entry-point") << hdr->e_entry;
    s.entry("relocatable") << is_rel(obj);
}

std::string name_of_index(std::size_t i) {
    std::ostringstream s;
    s << std::setfill('0') << std::setw(2) << i;
    return s.str();
}

template <typename I>
void program_headers(I begin, I end, ogre_doc &s) {
    std::size_t i = 0;
    for (auto it = begin; it != end; ++it, ++i) {
        bool ld = (it->p_type == ELF::PT_LOAD);
        bool r = static_cast<bool>(it->p_flags & ELF::PF_R);
        bool w = static_cast<bool>(it->p_flags & ELF::PF_W);
        bool x = static_cast<bool>(it->p_flags & ELF::PF_X);
        auto off = it->p_offset;
        auto filesz = it->p_filesz;
        auto name = name_of_index(i);
        s.entry("program-header") << name << off << filesz;
        s.entry("virtual-program-header") << name << it->p_vaddr << it->p_memsz;
        s.entry("program-header-flags") << name << ld << r << w << x;
    }
}

template <typename T>
void section_header(const T &hdr, const std::string &name, ogre_doc &s) {
    s.entry("section-header") << name << hdr.sh_addr << hdr.sh_size << hdr.sh_offset;
    bool w = static_cast<bool>(hdr.sh_flags & ELF::SHF_WRITE);
    bool x = static_cast<bool>(hdr.sh_flags & ELF::SHF_EXECINSTR);
    s.entry("section-flags") << name << w << x;
}

template <typename T>
void symbol_entry(const Elf_Sym_Impl<T> &sym, const std::string &name,
                  uint64_t addr, ogre_doc &s) {
    s.entry("symbol-entry") << name << addr << sym.st_size;
    if (sym.getType() == ELF::STT_FUNC)
        s.entry("code-entry") << addr << name;
}

template <typename T>
void symbol_reference(const ELFObjectFile<T> &obj, ELFRelocationRef &rel, uint64_t sec_offset, ogre_doc &s) {
    auto it = rel.getSymbol();
    auto sym_elf = obj.getSymbol(it->getRawDataRefImpl());
    auto off = sec_offset + rel.getOffset(); // full file offset where to apply relocation
    auto sec = it->getSection();
    if (sym_elf->getBinding() == ELF::STB_GLOBAL && sym_elf->st_size == 0) {
        auto sym_name = it->getName();
        if (sym_name)
            s.entry("external-symbol") << off << sym_name.get().str();
        return;
    }
    if (sec) {
        auto sec_elf = obj.getSection((*sec)->getRawDataRefImpl());
        auto addr = sec_elf->sh_offset + sym_elf->st_value;
        s.entry("symbol-reference") << off << addr << sym_elf->st_size;
    }
}


#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

template <typename T>
void program_headers(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto elf = obj.getELFFile();
    program_headers(elf->program_header_begin(), elf->program_header_end(), s);
}

template <typename T>
void section_headers(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto elf = obj.getELFFile();
    for (auto it = elf->section_begin(); it != elf->section_end(); ++it) {
        auto name = elf->getSectionName(it);
        if (!name) s.fail(name.getError().message());
        section_header(*it, name.get().str(), s);
    }
}

// We will treat a file offset of a symbol as an address in relocatable
// files. It consists from two parts in this case: symbol's value, which
// is a symbol offset within some section and offset of this section.
// (symbol's value is a section offset only for relocatable files)
template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, symbol_iterator begin, symbol_iterator end, ogre_doc &s) {
    for (auto it = begin; it != end; ++it) {
        ELFSymbolRef sym(*it);
        auto name = sym.getName();
        auto addr = sym.getAddress();
        if (!name || !addr) continue;
        auto sym_elf = obj.getSymbol(sym.getRawDataRefImpl());
        uint64_t address = addr.get();

        if (is_rel(obj)) {
            auto sec_it = it->getSection();
            // check that sec_it points to a real, but not to a "special" elf section
            if (sec_it && !(*sec_it == obj.section_end())) {
                auto sec_elf = obj.getSection((*sec_it)->getRawDataRefImpl());
                address += sec_elf->sh_offset;
            }
        }
        symbol_entry(*sym_elf, name.get().str(), address, s);
    }
}

template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, ogre_doc &s) {
    typedef typename ELFFile<T>::Elf_Shdr sec_hdr;
    auto elf = obj.getELFFile();
    symbol_entries(obj, obj.symbol_begin(), obj.symbol_end(), s);
    bool is_dyn = std::any_of(elf->section_begin(), elf->section_end(),
                              [](const sec_hdr &hdr) { return (hdr.sh_type == ELF::SHT_DYNSYM); });
    if (is_dyn) // preventing from llvm 3.8 fail in case of .dynsym absence
        symbol_entries(obj, obj.dynamic_symbol_begin(), obj.dynamic_symbol_end(), s);
}

bool contains_code(section_iterator it) {
    ELFSectionRef s(*it);
    return static_cast<bool>(s.getFlags() & ELF::SHF_EXECINSTR);
}

bool refered_to_code(SectionRef s) {
    return contains_code(s.getRelocatedSection());
}

template <typename T>
void relocations(const ELFObjectFile<T> &obj, ogre_doc &s) {
    for (auto sec : obj.sections()) {
        // if (refered_to_code(sec)) {
            auto rel = sec.getRelocatedSection();
            auto hdr = obj.getSection(rel->getRawDataRefImpl());
            for (auto rel : sec.relocations()) {
                ELFRelocationRef erel(rel);
                symbol_reference(obj, erel, hdr->sh_offset, s);
            }
//        }
    }
}




//TODO: remove it later
std::string binding(uint8_t x) {
    std::string s;
    switch (x) {
    case(ELF::STB_LOCAL) : s = "local"; break;
    case(ELF::STB_GLOBAL) : s = "global"; break;
    case(ELF::STB_WEAK) : s = "weak"; break;
    case(ELF::STB_LOOS) : s = "loos"; break;
    case(ELF::STB_HIOS) : s = "hios"; break;
    case(ELF::STB_LOPROC) : s = "loproc"; break;
    case(ELF::STB_HIPROC) : s = "hiproc"; break;
    default : s = "unknown";
    }
    return s;
}

//TODO: remove it later
std::string reloc_type(RelocationRef rel) {
    SmallString<16> name;
    rel.getTypeName(name);
    return name.str().str();
}

//TODO: remove it later
template <typename T>
std::string sym_type(const Elf_Sym_Impl<T> &sym) {
    std::string s;
    switch (sym.getType()) {
    case(ELF::STT_NOTYPE)  : s = "notype";  break;
    case(ELF::STT_OBJECT)  : s = "object";  break;
    case(ELF::STT_FUNC)    : s = "is_fun";  break;
    case(ELF::STT_SECTION) : s = "section"; break;
    case(ELF::STT_FILE)    : s = "file";    break;
    case(ELF::STT_COMMON)  : s = "common";  break;
    default                : s = "other";
    }
    return s;
}

//TODO: remove it later
std::string secname(SectionRef s) {
    StringRef name;
    auto er = s.getName(name);
    if (!er) return name.str();
    else return "noname section";
}


//TODO: remove it later
template <typename T>
void explore_rel(const ELFObjectFile<T> &obj) {
    auto visitor = RelocVisitor(obj);
    for (auto sec : obj.sections()) {
        std::cout << "-- explore section " << secname(sec) << " ----" << std::endl;
        std::cout << "-- refered to " << secname(*(sec.getRelocatedSection()))
                  << std::boolalpha << "; refered to code? " << refered_to_code(sec) << std::endl;
        for (auto rel : sec.relocations()) {
            ELFRelocationRef erel(rel);
            auto it = erel.getSymbol();
            auto sym_elf = obj.getSymbol(it->getRawDataRefImpl());
            //rel type, rel offset
            auto add = erel.getAddend();
            auto off = erel.getOffset();
            std::cout << reloc_type(rel) << "; off " << std::hex << "0x"
                      << off << std::dec << "(" << off << ")";
            // info about symbol: name, size
            auto sym_name = it->getName();
            if (sym_name) {
                std::cout << "(ref to sym " <<  sym_name.get().str() << " of size "
                          << sym_elf->st_size << " with sym_value " << sym_elf->st_value;
                //check symbol type
                std::cout << "; shndx " << sym_elf->st_shndx;
                std::cout << "; value " << sym_elf->st_value;
                //symbol binding
                std::cout << "; binding " << binding(sym_elf->getBinding());
                std::cout << "; type " << sym_type(*sym_elf) << ")";
            }
            //addend
            if (add)
                std::cout << "; addend: " << *add;
            else
                std::cout << "; no addend";
            //section info
            auto sec = it->getSection();
            if (sec) {
                StringRef secname;
                auto er = sec.get()->getName(secname);
                if (!er)
                    std::cout <<  "; section " << secname.str();
                else
                    std::cout <<  "; noname section";
                auto sec_elf = obj.getSection((*sec)->getRawDataRefImpl());
                std::cout << "; section off " << sec_elf->sh_offset;
            } else { std::cout <<  "; no section "; }
            //apply relocation
            auto app = visitor.visit(rel.getType(), rel);
            std::cout << "; value: " << app.Value << "; width " << int(app.Width);
            std::cout << std::endl;
        }
        std::cout << "--------" << std::endl;
    }
}




#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

template <typename T>
void program_headers(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto elf = obj.getELFFile();
    program_headers(elf->begin_program_headers(), elf->end_program_headers(), s);
}

template <typename T>
void section_headers(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto elf = obj.getELFFile();
    for (auto it = elf->begin_sections(); it != elf->end_sections(); ++it) {
        auto name = elf->getSectionName(&*it);
        if (name)
            section_header(*it, (*name).str(), s);
        else
            s.fail(error_code(name).message());
    }
}

//TODO: write relocations too
template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, symbol_iterator begin, symbol_iterator end, ogre_doc &s) {
    StringRef name;
    uint64_t addr, size;
    section_iterator sec_it;
    SymbolRef::Type typ;
    for (auto it = begin; it != end; next(it, end)) {
        auto er_name = it->getName(name);
        auto er_addr = it->getAddress(addr);
        if (er_name || er_addr) continue;
        auto sym_elf = obj.getSymbol(it->getRawDataRefImpl());
        symbol_entry(*sym_elf, name.str(), addr, s);
        if (is_rel(obj)) {
            section_iterator sec_it;
            auto er_sec = it->getSection(sec_it);
            if (er_sec) return;
            auto sec_elf = obj.toELFShdrIter(sec_it->getRawDataRefImpl());
            relocatable_symbol(*sym_elf, name.get().str(), it->getValue(), sec_elf->sh_offset, s);
        }
    }
}

template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, ogre_doc &s) {
    typedef typename ELFFile<T>::Elf_Shdr sec_hdr;
    auto elf = obj.getELFFile();
    symbol_entries(obj, obj.begin_symbols(), obj.end_symbols(), s);
    symbol_entries(obj, obj.begin_dynamic_symbols(), obj.begin_dynamic_symbols(), s);
}
#else
#error LLVM version is not supported
#endif

} // namespace elf_loader

template <typename T>
error_or<std::string> load(const llvm::object::ELFObjectFile<T> &obj) {
    using namespace elf_loader;
    ogre_doc s;
    s.raw_entry(elf_declarations);
    s.raw_entry("(file-type elf)");
    s.entry("arch") << arch_of_object(obj);
    file_header(obj, s);
    program_headers(obj, s);
    section_headers(obj, s);
    symbol_entries(obj, s);
    relocations(obj, s);

    explore_rel(obj);

    return s.str();
}

} // namespace loader

#endif // LLVM_ELF_LOADER_HPP
