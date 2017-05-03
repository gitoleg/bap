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

void arch(const ObjectFile& obj, std::ostringstream &s) {
    s << (sexp("arch") <<
        Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch())));
}

template <typename T>
void file_header(const ELFObjectFile<T> &obj, std::ostringstream &s) {
    auto hdr = obj.getELFFile()->getHeader();
    s << (sexp("entry-point") << hdr->e_entry);
}

template <typename I>
void program_headers(I begin, I end, std::ostringstream &s) {
    for (auto it = begin; it != end; ++it) {
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
void section_header(const T &hdr, const std::string &name, std::ostringstream &s) {
    s << (sexp("section-header") << quoted(name) << hdr.sh_addr << hdr.sh_size);
}

void symbol_entry(const std::string &name, uint64_t addr, uint64_t size,
                  const SymbolRef::Type &typ, std::ostringstream &s) {
    bool is_fun = (typ == SymbolRef::ST_Function);
    s << (sexp("symbol-entry") << quoted(name) << addr << size << is_fun);
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

template <typename T>
void program_headers(const ELFObjectFile<T> &obj, std::ostringstream &s) {
    auto elf = obj.getELFFile();
    program_headers(elf->program_header_begin(), elf->program_header_end(), s);
}

template <typename T>
void section_headers(const ELFObjectFile<T> &obj, std::ostringstream &s) {
    auto elf = obj.getELFFile();
    for (auto it = elf->section_begin(); it != elf->section_end(); ++it)
        if (auto name = elf->getSectionName(it))
            section_header(*it, name.get().str(), s);
}

template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, symbol_iterator begin, symbol_iterator end, std::ostringstream &s) {
    for (auto it = begin; it != end; ++it) {
        ELFSymbolRef sym(*it);
        auto name = sym.getName();
        auto addr = sym.getAddress();
        if (!name || !addr) continue;
        symbol_entry(name.get().str(), addr.get(), sym.getSize(), sym.getType(), s);
    }
}

template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, std::ostringstream &s) {
    typedef typename ELFFile<T>::Elf_Shdr sec_hdr;
    auto elf = obj.getELFFile();
    symbol_entries(obj, obj.symbol_begin(), obj.symbol_end(), s);
    bool is_dyn = std::any_of(elf->section_begin(), elf->section_end(),
                              [](const sec_hdr &hdr) { return (hdr.sh_type == ELF::SHT_DYNSYM); });
    if (is_dyn) // primary preventing from llvm 3.8 fail in case of .dynsym absence
        symbol_entries(obj, obj.dynamic_symbol_begin(), obj.dynamic_symbol_end(), s);
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

template <typename T>
void program_headers(const ELFObjectFile<T> &obj, std::ostringstream &s) {
    auto elf = obj.getELFFile();
    program_headers(elf->begin_program_headers(), elf->end_program_headers(), s);
}

//almost copy - will see how getSectionName will work
template <typename T>
void section_headers(const ELFObjectFile<T> &obj, std::ostringstream &s) {
    auto elf = obj.getELFFile();
    for (auto it = elf->begin_sections(); it != elf->end_sections(); ++it)
        if (auto name = elf->getSectionName(it))
            section_header(*it, name.get().str(), s);
}

template <typename I>
void next(I &i, I end) {
    error_code ec;
    it.increment(ec);
    if (ec) it = end;
}

template <typename T>
void symbol_entries(symbol_iterator begin, symbol_iterator end, std::ostringstream &s) {
    StringRef name;
    uint64_t addr,size;
    SymbolRef::Type typ;
    for (auto it = begin; it != end; next(it, end)) {
        auto er_name = it->getName(name);
        auto er_addr = it->getAddress(addr);
        auto er_size = it->getAddress(size);
        auto er_type = it->getAddress(type);
        if (er_name || er_addr || er_size || er_type) continue;
        symbol_entry(name.str(), addr, size, typ);
    }
}

template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, std::ostringstream &s) {
    typedef typename ELFFile<T>::Elf_Shdr sec_hdr;
    auto elf = obj.getELFFile();
    symbol_entries(obj.begin_symbols(), obj.end_symbols(), s);
    symbol_entries(obj.begin_dynamic_symbols(), obj.begin_dynamic_symbols(), s);
}
#else
#error LLVM version is not supported
#endif

static const std::string declarations =
    "(declare arch (name str))"
    "(declare entry-point (addr int))"
    "(declare program-header (offset int) (size int))"
    "(declare virtual-pheader (offset int) (size int) (v-addr int) (v-size int))"
    "(declare pheader-flags (offset int) (size int) (load bool) (read bool) (write bool) (execute bool))"
    "(declare section-header (name str) (v-addr int) (size int))"
    "(declare symbol-entry (name str) (v-addr int) (size int) (is-function bool))";

//TODO: rework returning type .. possible
template <typename T>
error_or<std::string> load(const ELFObjectFile<T> &obj) {
    std::ostringstream s;
    s << declarations;
    arch(obj, s);
    file_header(obj, s);
    program_headers(obj, s);
    section_headers(obj, s);
    symbol_entries(obj, s);
    std::string res = s.str();
    return success(res);
}

} // namespace loader

#endif // LLVM_ELF_LOADER_HPP
