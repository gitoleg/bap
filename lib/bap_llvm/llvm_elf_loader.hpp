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

#include <iostream>
#include <iomanip>

#include <llvm/Object/ELFObjectFile.h>

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
    "(declare symbol-reference (offset int) (addr int))"
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
bool is_external_symbol(const T &sym) {
    return (sym.getBinding() == ELF::STB_GLOBAL && sym.st_size == 0);
}

template <typename T>
bool is_abs_symbol(const T &sym) {
    return (sym.st_shndx == ELF::SHN_ABS);
}

template <typename T>
uint64_t section_offset(const ELFObjectFile<T> &obj, section_iterator it);
uint64_t relocation_offset(const RelocationRef &rel, uint64_t sec_offset);

// few primitives, returns true if everything is ok

bool symbol_name(const SymbolRef &s, std::string &name);

bool symbol_address(const SymbolRef &s, uint64_t &address);

template <typename T>
bool symbol_file_offset(const ELFObjectFile<T> &obj, const SymbolRef &sym, uint64_t &off);

// We will treat a file offset of a symbol as an address in relocatable
// files. It consists from two parts in this case: symbol's value, which
// is a symbol offset within some section and offset of this section.
// (symbol's value is a section offset only for relocatable files)
template <typename T>
bool symbol_address(const ELFObjectFile<T> &obj, const SymbolRef &sym, uint64_t &addr) {
    auto sym_elf = obj.getSymbol(sym.getRawDataRefImpl());
    if (is_rel(obj) && !is_abs_symbol(*sym_elf))  // abs symbols does not affected by relocations
        return symbol_file_offset(obj, sym, addr);
    else
        return symbol_address(sym, addr);
}

template <typename T>
void symbol_reference(const ELFObjectFile<T> &obj, const RelocationRef &rel, section_iterator sec, ogre_doc &s) {
    auto it = rel.getSymbol();
    auto sym_elf = obj.getSymbol(it->getRawDataRefImpl());
    auto sec_offset = section_offset(obj, sec);
    auto off = relocation_offset(rel, sec_offset);
    if (is_external_symbol(*sym_elf)) {
        std::string sym_name;
        if (symbol_name(*it, sym_name))
            s.entry("external-symbol") << off << sym_name;
    } else {
        uint64_t file_offset;
        if (symbol_file_offset(obj, *it, file_offset))
            s.entry("symbol-reference") << off << file_offset;
    }
}

template <typename T>
void symbol_entry(const ELFObjectFile<T> &obj, const SymbolRef &sym, ogre_doc &s) {
    uint64_t addr;
    std::string name;
    auto sym_elf = obj.getSymbol(sym.getRawDataRefImpl());
    if (symbol_name(sym, name) && symbol_address(obj, sym, addr)) {
        s.entry("symbol-entry") << name << addr << sym_elf->st_size;
        if (sym_elf->getType() == ELF::STT_FUNC)
            s.entry("code-entry") << addr << name;
    }
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

bool symbol_address(const SymbolRef &sym, uint64_t &address) {
    auto addr = sym.getAddress();
    if (addr) address = *addr;
    return (bool)addr;
}

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

template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, symbol_iterator begin, symbol_iterator end, ogre_doc &s) {
    for (auto it = begin; it != end; ++it)
        symbol_entry(obj, *it, s);
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

uint64_t relocation_offset(const RelocationRef &rel, uint64_t sec_offset) {
    return rel.getOffset() + sec_offset;
}

bool symbol_name(const SymbolRef &s, std::string &name) {
    auto sym_name = s.getName();
    if (sym_name)
        name = sym_name.get().str();
    return (bool)sym_name;
}

template <typename T>
bool symbol_file_offset(const ELFObjectFile<T> &obj, const SymbolRef &sym, uint64_t &off) {
    auto sec = sym.getSection();
    auto addr = sym.getAddress();
    bool check = sec && addr;
    if (check)
        off = *addr + section_offset(obj, *sec);
    return check;
}


template <typename T>
void relocations(const ELFObjectFile<T> &obj, ogre_doc &s) {
    for (auto sec : obj.sections()) {
        auto rel_sec = sec.getRelocatedSection();
        for (auto rel : sec.relocations())
            symbol_reference(obj, rel, rel_sec, s);
    }
}

template <typename T>
uint64_t section_offset(const ELFObjectFile<T> &obj, section_iterator it) {
    if (it == obj.section_end()) return 0; // check for special elf sections
    auto sec_elf = obj.getSection(it->getRawDataRefImpl());
    return sec_elf->sh_offset;
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

template <typename T>
uint64_t section_offset(const ELFObjectFile<T> &obj, section_iterator it) {
    typedef typename ELFObjectFile<T>::Elf_Shdr_Iter elf_shdr_iterator;

    if (it == obj.end_sections()) return 0; // check for special elf sections

    auto elf = obj.getELFFile();
    auto raw = it->getRawDataRefImpl();
    auto elf_sec_it = elf_shdr_iterator(elf->getHeader()->e_shentsize,
                                        reinterpret_cast<const char *>(raw.p));
    return elf_sec_it->sh_offset;
}

uint64_t relocation_offset(const RelocationRef &rel, uint64_t sec_offset) {
    uint64_t off;
    auto er_off = rel.getOffset(off); // it's always successful operation
    return sec_offset + off;
}

bool symbol_name(const SymbolRef &s, std::string &name) {
    StringRef name_ref;
    auto er_name = s.getName(name_ref);
    if (!er_name)
        name = name_ref.str();
    return !er_name;
}

bool symbol_address(const SymbolRef &s, uint64_t &addr) {
    auto er = s.getAddress(addr);

    //need to perform this check due to nice llvm code like:
    // ...
    // Result = UnknownAddressOrSize;
    // return object_error::success;
    // ..
    // where UnknownAddressOrSize = 18446744073709551615
    if (addr == UnknownAddressOrSize)
        addr = 0;
    return !er;
}

template <typename T>
bool symbol_file_offset(const ELFObjectFile<T> &obj, const SymbolRef &sym, uint64_t &off) {
    section_iterator sec = obj.begin_sections();
    uint64_t addr;
    auto er_sec  = sym.getSection(sec);
    auto er_addr = sym.getAddress(addr);
    bool check = !er_sec && !er_addr;
    if (check)
        off = addr + section_offset(obj, sec);
    return check;
}


template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, symbol_iterator begin, symbol_iterator end, ogre_doc &s) {
    for (auto it = begin; it != end; next(it, end))
        symbol_entry(obj, *it, s);
}

template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, ogre_doc &s) {
    typedef typename ELFFile<T>::Elf_Shdr sec_hdr;
    auto elf = obj.getELFFile();
    symbol_entries(obj, obj.begin_symbols(), obj.end_symbols(), s);
    symbol_entries(obj, obj.begin_dynamic_symbols(), obj.begin_dynamic_symbols(), s);
}

template <typename T>
void relocations(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto end = obj.end_sections();
    for (auto sec_it = obj.begin_sections(); sec_it != end; next(sec_it, end)) {
        auto rel_sec = sec_it->getRelocatedSection();
        auto rel_end = sec_it->end_relocations();
        for (auto rel_it = sec_it->begin_relocations(); rel_it != rel_end; next(rel_it, rel_end))
            symbol_reference(obj, *rel_it, rel_sec, s);
    }
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
    return s.str();
}

} // namespace loader

#endif // LLVM_ELF_LOADER_HPP
