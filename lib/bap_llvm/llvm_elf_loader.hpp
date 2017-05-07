#ifndef LLVM_ELF_LOADER_HPP
#define LLVM_ELF_LOADER_HPP

#include <iostream>
#include <sstream>
#include <iomanip>

#include <llvm/Object/ELFObjectFile.h>

#include "llvm_error_or.hpp"

namespace loader {

using namespace llvm;
using namespace llvm::object;

struct data_stream {

    explicit data_stream() : s_(std::ostringstream()) {}

    void fail(const std::string &m) { s_.fail(m); }

    template <typename T>
    friend data_stream & operator<<(data_stream &s, const T &t) {
        if (s.s_) *s.s_ << t;
        return s;
    }

    error_or<std::string> str() const {
        if (s_) return success(s_->str());
        else return failure(s_.message());
    }

private:
    error_or<std::ostringstream> s_;
};


std::string quoted(const std::string &s) {
    return "\"" + s + "\"";
}

static const std::string declarations =
    "(declare arch (name str))"
    "(declare entry-point (addr int))"
    "(declare program-header (offset int) (size int) (name str))"
    "(declare virtual-pheader (offset int) (size int) (addr int) (v-size int))"
    "(declare pheader-flags (offset int) (size int) (load bool) (read bool) (write bool) (execute bool))"
    "(declare section-header (name str) (addr int) (size int))"
    "(declare symbol-entry (name str) (addr int) (size int) (function bool))";

template <typename T>
void arch(const ELFObjectFile<T> &obj, data_stream &s) {
    s << "(arch " << Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch())) << ")";
}

template <typename T>
void file_header(const ELFObjectFile<T> &obj, data_stream &s) {
    auto hdr = obj.getELFFile()->getHeader();
    s << "(entry-point " << hdr->e_entry << ")";
}

std::string name_of_index(std::size_t i) {
    std::ostringstream s;
    s << std::setfill('0') << std::setw(2) << i;
    return s.str();
}

template <typename I>
void program_headers(I begin, I end, data_stream &s) {
    std::size_t i = 0;
    for (auto it = begin; it != end; ++it, ++i) {
        bool ld = (it->p_type == ELF::PT_LOAD);
        bool r = static_cast<bool>(it->p_flags & ELF::PF_R);
        bool w = static_cast<bool>(it->p_flags & ELF::PF_W);
        bool x = static_cast<bool>(it->p_flags & ELF::PF_X);
        auto off = it->p_offset;
        auto filesz = it->p_filesz;
        auto name = name_of_index(i);
        s << "(program-header "  << off << " " << filesz << " " << name << ")";
        s << "(virtual-pheader " << off << " " << filesz << " " << it->p_vaddr << " " << it->p_memsz << ")";
        s << "(pheader-flags "   << off << " " << filesz << " " << ld << " " << r << " " <<  w << " " << x  << ")";
    }
}

template <typename T>
void section_header(const T &hdr, const std::string &name, data_stream &s) {
    s << "(section-header " << quoted(name) << " " << hdr.sh_addr << " " << hdr.sh_size << ")";
}

void symbol_entry(const std::string &name, uint64_t addr, uint64_t size,
                  const SymbolRef::Type &typ, data_stream &s) {
    bool is_fun = (typ == SymbolRef::ST_Function);
    s << "(symbol-entry " << quoted(name) << " " << addr << " " << size << " " << is_fun << ")";
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

template <typename T>
void program_headers(const ELFObjectFile<T> &obj, data_stream &s) {
    auto elf = obj.getELFFile();
    program_headers(elf->program_header_begin(), elf->program_header_end(), s);
}

template <typename T>
void section_headers(const ELFObjectFile<T> &obj, data_stream &s) {
    auto elf = obj.getELFFile();
    for (auto it = elf->section_begin(); it != elf->section_end(); ++it) {
        auto name = elf->getSectionName(it);
        if (name)
            section_header(*it, name.get().str(), s);
        else
            s.fail(name.getError().message());
    }
}

void symbol_entries(symbol_iterator begin, symbol_iterator end, data_stream &s) {
    for (auto it = begin; it != end; ++it) {
        ELFSymbolRef sym(*it);
        auto name = sym.getName();
        auto addr = sym.getAddress();
        if (!name || !addr) continue;
        symbol_entry(name.get().str(), addr.get(), sym.getSize(), sym.getType(), s);
    }
}

template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, data_stream &s) {
    typedef typename ELFFile<T>::Elf_Shdr sec_hdr;
    auto elf = obj.getELFFile();
    symbol_entries(obj.symbol_begin(), obj.symbol_end(), s);
    bool is_dyn = std::any_of(elf->section_begin(), elf->section_end(),
                              [](const sec_hdr &hdr) { return (hdr.sh_type == ELF::SHT_DYNSYM); });
    if (is_dyn) // preventing from llvm 3.8 fail in case of .dynsym absence
        symbol_entries(obj.dynamic_symbol_begin(), obj.dynamic_symbol_end(), s);
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

template <typename T>
void program_headers(const ELFObjectFile<T> &obj, data_stream &s) {
    auto elf = obj.getELFFile();
    program_headers(elf->begin_program_headers(), elf->end_program_headers(), s);
}

template <typename T>
void section_headers(const ELFObjectFile<T> &obj, data_stream &s) {
    auto elf = obj.getELFFile();
    for (auto it = elf->begin_sections(); it != elf->end_sections(); ++it) {
        auto name = elf->getSectionName(&*it);
        if (name)
            section_header(*it, (*name).str(), s);
        else
            s.fail(name.message());
    }
}

template <typename I>
void next(I &it, I end) {
    error_code ec;
    it.increment(ec);
    if (ec) it = end;
}

void symbol_entries(symbol_iterator begin, symbol_iterator end, data_stream &s) {
    StringRef name;
    uint64_t addr, size;
    SymbolRef::Type typ;
    for (auto it = begin; it != end; next(it, end)) {
        auto er_name = it->getName(name);
        auto er_addr = it->getAddress(addr);
        auto er_size = it->getAddress(size);
        auto er_type = it->getType(typ);
        if (er_name || er_addr || er_size || er_type) continue;
        symbol_entry(name.str(), addr, size, typ, s);
    }
}

template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, data_stream &s) {
    typedef typename ELFFile<T>::Elf_Shdr sec_hdr;
    auto elf = obj.getELFFile();
    symbol_entries(obj.begin_symbols(), obj.end_symbols(), s);
    symbol_entries(obj.begin_dynamic_symbols(), obj.begin_dynamic_symbols(), s);
}
#else
#error LLVM version is not supported
#endif

template <typename T>
error_or<std::string> load(const ELFObjectFile<T> &obj) {
    data_stream s;
    s << std::boolalpha << declarations;
    arch(obj, s);
    file_header(obj, s);
    program_headers(obj, s);
    section_headers(obj, s);
    symbol_entries(obj, s);
    return s.str();
}

} // namespace loader

#endif // LLVM_ELF_LOADER_HPP
