#ifndef LLVM_ELF_LOADER_HPP
#define LLVM_ELF_LOADER_HPP

#include <iostream>
#include <iomanip>
#include <sstream>
#include <tuple>

#include <llvm/Object/ELFObjectFile.h>
#include "llvm_error_or.hpp"
#include "llvm_loader_scheme.hpp"

namespace elf_loader {

using namespace llvm;
using namespace llvm::object;

typedef error_or<std::ostringstream> ostream;

void provide_section(ostream &s, const std::string &name, uint64_t addr, uint64_t size) {
    *s << scheme::section(addr, size) << scheme::named_region(addr, size, name);
}

void provide_symbol(ostream &s, const std::string &name, uint64_t addr, uint64_t size, bool is_fun) {
    *s << scheme::named_symbol(addr, name);
    if (is_fun) {
        *s << scheme::code_start(addr);
        *s << scheme::symbol_chunk(addr, size, addr);
    }
}

template <typename T>
void skip_symbol(ostream &s, const T &er) {
    s.warning() << "skipping symbol: " << er.message();
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

template <typename T>
const typename ELFFile<T>::Elf_Phdr* header_begin(const ELFFile<T> *elf) {
    return elf->program_header_begin();
}

template <typename T>
const typename ELFFile<T>::Elf_Phdr* header_end(const ELFFile<T> *elf) {
    return elf->program_header_end();
}

template <typename T>
void provide_sections(ostream &s, const ELFObjectFile<T> &obj) {
    for (auto sec : obj.sections()) {
        StringRef name;
        if (auto er = sec.getName(name)) { s.fail(er.message()); return; };
        provide_section(s, name.str(), sec.getAddress(), sec.getSize());
    }
}

void provide_symbol(ostream &s, const SymbolRef &gen_sym) {
    ELFSymbolRef sym(gen_sym);
    auto name = sym.getName();
    auto addr = sym.getAddress();
    if (!name) { skip_symbol(s, name.getError()); return; }
    if (!addr) { skip_symbol(s, addr.getError()); return; }
    provide_symbol(s, name.get().str(), addr.get(), sym.getSize(),
                   (sym.getType() == SymbolRef::ST_Function));
}

template <typename T>
void provide_symbols(ostream &s, const ELFObjectFile<T> &obj) {
    typedef typename ELFFile<T>::Elf_Shdr sec_hdr;
    std::size_t sym_count = std::distance(obj.symbol_begin(), obj.symbol_end());
    auto sections = obj.getELFFile()->sections();
    bool is_dyn = std::any_of(sections.begin(), sections.end(),
                              [](const sec_hdr &hdr) { return (hdr.sh_type == ELF::SHT_DYNSYM); });

    for (auto it = obj.symbol_begin(); it != obj.symbol_end(); ++it)
        provide_symbol(s, *it);
//  due to a bug in llvm 3.8 with stripped static binaries, we have to perform such check
    if  (!sym_count && !is_dyn)
        return;
    for (auto it = obj.dynamic_symbol_begin(); it != obj.dynamic_symbol_end(); ++it)
        provide_symbol(s, *it);
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

template <typename T>
const typename ELFFile<T>::Elf_Phdr_Iter header_begin(const ELFFile<T> *elf) {
    return elf->begin_program_headers();
}

template <typename T>
const typename ELFFile<T>::Elf_Phdr_Iter header_end(const ELFFile<T> *elf) {
    return elf->end_program_headers();
}

template <typename T>
void provide(ostream &s, const T &t) {}

template <>
void provide(ostream &s, const SectionRef &sec) {
    StringRef name;
    uint64_t addr, size;
    if (auto er = sec.getName(name))    { return s.fail(er.message()); }
    if (auto er = sec.getAddress(addr)) { return s.fail(er.message()); }
    if (auto er = sec.getSize(size))    { return s.fail(er.message()); }
    provide_section(s, name.str(), addr, size);
}

template <>
void provide(ostream &s, const SymbolRef &sym) {
    StringRef name;
    uint64_t addr, size;
    SymbolRef::Type kind;
    if (auto er = sym.getName(name))    { skip_symbol(s, er); return; }
    if (auto er = sym.getAddress(addr)) { skip_symbol(s, er); return; }
    if (auto er = sym.getSize(size))    { skip_symbol(s, er); return; }
    if (auto er = sym.getType(kind))    { skip_symbol(s, er); return; }
    provide_symbol(s, name.str(), addr, size, (kind == SymbolRef::ST_Function));
}

template <typename T>
void iter(content_iterator<T> first, content_iterator<T> last, ostream &s) {
    error_code er;
    while (first != last) {
        provide(*first, s);
        first.increment(er);
        if (er) {
            s.fail(er.message());
            return;
        }
    }
}

template <typename T>
void provide_symbols(ostream &s, const ELFObjectFile<T> &obj) {
    iter(obj.begin_symbols(), obj.begin_symbols(), s);
    iter(obj.begin_dynamic_symbols(), obj.begin_dynamic_symbols(), s);
}

template <typename T>
void provide_sections(ostream &s, const ELFObjectFile<T> &obj) {
    iter(obj.begin_sections(), obj.end_sections(), s);
}

#else
#error LLVM version is not supported
#endif

template <typename T>
void provide_segments(ostream &s, const ELFObjectFile<T> &obj) {
    auto begin = header_begin(obj.getELFFile());
    auto end = header_end(obj.getELFFile());
    auto it = begin;
    for (int pos = 0; it != end; ++it, ++pos) {
        if (it -> p_type == ELF::PT_LOAD) {
            std::ostringstream oss;
            oss << std::setfill('0') << std::setw(2) << pos;
            int offset = it->p_filesz == 0 ? -1 : it->p_offset;
            *s << scheme::segment(it->p_vaddr, it->p_filesz,
                                        static_cast<bool>(it->p_flags & ELF::PF_R),
                                        static_cast<bool>(it->p_flags & ELF::PF_W),
                                        static_cast<bool>(it->p_flags & ELF::PF_X));
            *s << scheme::mapped(it->p_vaddr, it->p_filesz, offset);
            *s << scheme::named_region(it->p_vaddr, it->p_filesz, oss.str());
        }
    }
}

template <typename T>
error_or<std::string> load(const ELFObjectFile<T> &obj) {
    ostream s = success(std::ostringstream());
    auto arch_str = Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch()));
    *s << scheme::declare();
    *s << scheme::arch(arch_str);
    *s << scheme::entry_point(obj.getELFFile()->getHeader()->e_entry);
    provide_segments(s, obj);
    provide_sections(s, obj);
    if (!s) return failure(s.message());
    provide_symbols(s, obj);
    if (!s) return failure(s.message());
    return std::move(success(s->str()) << s.warnings());
}

} // namespace elf_loader

#endif // LLVM_ELF_LOADER_HPP
