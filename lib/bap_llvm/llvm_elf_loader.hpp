#ifndef LLVM_ELF_LOADER_HPP
#define LLVM_ELF_LOADER_HPP

#include <iostream>
#include <sstream>

#include <llvm/Object/ELFObjectFile.h>

#include "llvm_error_or.hpp"
#include "llvm_loader_utils.hpp"

namespace loader {

using namespace llvm;
using namespace llvm::object;


template <typename T>
struct elf_wrapper {
    typedef ELFObjectFile<T> elf_obj;
    typedef ELFFile<T> elf_file;
    explicit elf_wrapper(const elf_obj &e) : e_(e) {}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
    typedef const typename ELFFile<T>::Elf_Phdr phdr;
    typedef const typename ELFFile<T>::Elf_Shdr shdr;
    const phdr* pheaders_begin() const { return file().program_header_begin(); }
    const phdr* pheaders_end() const { return file().program_header_end(); }
    const shdr* begin_sections() const { return file().section_begin();}
    const shdr* end_sections() const { return file().section_end();}
    symbol_iterator begin_symbols() const { return e_.symbol_begin(); }
    symbol_iterator end_symbols() const { return e_.symbol_end(); }

    symbol_iterator begin_dynamic_symbols() const {
        bool is_dyn = std::any_of(begin_sections(), end_sections(),
                                  [](const shdr &hdr) { return (hdr.sh_type == ELF::SHT_DYNSYM); });
        if (is_dyn) return e_.dynamic_symbol_begin();
        else return e_.dynamic_symbol_end();
    }

    symbol_iterator end_dynamic_symbols() const { return e_.dynamic_symbol_end(); }
    ErrorOr<StringRef> symbol_name(symbol_iterator it) const { return it->getName();}
    ErrorOr<uint64_t> symbol_addr(symbol_iterator it) const { return it->getAddress(); }
    uint64_t symbol_size(symbol_iterator it) const { return ELFSymbolRef(*it).getSize(); }
    SymbolRef::Type symbol_type(symbol_iterator it) const { return it->getType(); }

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4
    typedef const typename ELFFile<T>::Elf_Phdr_ITer phdr;
    typedef const typename ELFFile<T>::Elf_Shdr_Iter shdr;
    phdr pheaders_begin() const { return file().begin_program_headers(); }
    phdr pheaders_end() const  { return file().end_program_headers(); }
    shdr begin_sections() const { return file().begin_sections();}
    shdr end_sections() const  { return file().end_sections();}
    symbol_iterator begin_symbols() const { return e_.begin_symbols(); }
    symbol_iterator end_symbols() const { return e_.end_symbols(); }
    symbol_iterator begin_dynamic_symbols() const { return e_.begin_dynamic_symbols(); }
    symbol_iterator end_dynamic_symbols() const { return e_.end_dynamic_symbols(); }

    error_or<StringRef> symbol_name(symbol_iterator it) const {
        StringRef name;
        if (auto ec = it->getName(name)) return failure(ec.message());
        else return success(name);
    }

    error_or<uint64_t> symbol_addr(symbol_iterator it) const {
        uint64_t addr;
        auto ec = it->getAddress(addr); // this is a safe op according to a code
        return success(addr);
    }

    uint64_t symbol_size(symbol_iterator it) const {
        uint64_t size;
        auto ec = it->getSize(size); // this is a safe op according to a code
        return size;
    }

    SymbolRef::Type symbol_type(symbol_iterator it) const {
        SymbolRef::Type typ;
        auto ec = it->getType(typ); // this is a safe op according to a code
        return typ;
    }

#else
#error LLVM version is not supported
#endif

    const elf_obj & origin() const { return e_; }
    const elf_file & file() const { return *(e_.getELFFile()); }

private:
    const ELFObjectFile<T> &e_;
};

static const std::string declarations =
    "(declare arch (name str))"
    "(declare entry-point (addr int))"
    "(declare program-header (offset int) (size int))"
    "(declare virtual-pheader (offset int) (size int) (v-addr int) (v-size int))"
    "(declare pheader-flags (offset int) (size int) (load bool) (read bool) (write bool) (execute bool))"
    "(declare section-header (name str) (v-addr int) (size int))"
    "(declare symbol-entry (name str) (v-addr int) (size int) (is-function bool))";

template <typename T>
void file_header(const ELFFile<T> &elf, std::ostringstream &s) {
    auto hdr = elf.getHeader();
    s << (sexp("entry-point") << hdr->e_entry);
}

template <typename T>
void program_headers(const elf_wrapper<T> &elf, std::ostringstream &s) {
    for (auto it = elf.pheaders_begin(); it != elf.pheaders_end(); ++it) {
        bool ld = (it->p_type == ELF::PT_LOAD);
        bool r = static_cast<bool>(it->p_flags & ELF::PF_R);
        bool w = static_cast<bool>(it->p_flags & ELF::PF_W);
        bool x = static_cast<bool>(it->p_flags & ELF::PF_X);
        s << (sexp("program-header")  << it->p_offset << it->p_filesz);
        s << (sexp("virtual-pheader") << it->p_offset << it->p_filesz << it->p_vaddr << it->p_memsz);
        s << (sexp("pheader-flags")   << it->p_offset << it->p_filesz << ld << r << w << x);
    }
}

template <typename T>
void section_headers(const elf_wrapper<T> &elf, std::ostringstream &s) {
    for (auto it = elf.begin_sections(); it != elf.end_sections(); ++it)
        if (auto name = elf.file().getSectionName(it))
            s << (sexp("section-header") << quoted(name.get().str()) << it->sh_addr << it->sh_size);
}

template <typename T>
void symbol_entries(const elf_wrapper<T> &elf, symbol_iterator begin, symbol_iterator end, std::ostringstream &s) {
    for (auto it = begin; it != end; ++it) {
        auto name = elf.symbol_name(it);
        auto addr = elf.symbol_addr(it);
        if (!name || !addr) return;
        bool is_fun = (elf.symbol_type(it) == SymbolRef::ST_Function);
        s << (sexp("symbol-entry") << quoted(name.get().str()) << addr.get() << elf.symbol_size(it) << is_fun);
    }
}

template <typename T>
void symbol_entries(const elf_wrapper<T> &elf, std::ostringstream &s) {
    symbol_entries(elf, elf.begin_symbols(), elf.end_symbols(), s);
    symbol_entries(elf, elf.begin_dynamic_symbols(), elf.end_dynamic_symbols(), s);
}

void arch(const ObjectFile& obj, std::ostringstream &s) {
    s << (sexp("arch") <<
        Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch())));
}


//TODO: rework returning type .. possible
template <typename T>
error_or<std::string> load(const ELFObjecctFile<T> &obj) {
    elf_wrapper<T> elf(obj);
    std::ostringstream s;
    s << declarations;
    arch(obj, s);
    file_header(elf.file(), s);
    program_headers(elf, s);
    section_headers(elf, s);
    symbol_entries(elf, s);
    std::string res = s.str();
    return success(res);
}

} // namespace loader

#endif // LLVM_ELF_LOADER_HPP
