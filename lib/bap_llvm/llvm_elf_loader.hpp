#ifndef LLVM_ELF_LOADER_HPP
#define LLVM_ELF_LOADER_HPP

#include <sstream>
#include <iomanip>
#include <sstream>
#include <tuple>

#include <llvm/Object/ELFObjectFile.h>
#include "llvm_error_or.hpp"
#include "llvm_loader_utils.hpp"

namespace loader {

namespace elf {

using namespace llvm;
using namespace llvm::object;

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
void provide_sections(utils::ostream &s, const ELFObjectFile<T> &obj) {
    for (auto sec : obj.sections()) {
        StringRef name;
        if (auto er = sec.getName(name)) { s.fail(er.message()); return; };
        utils::provide_section(s, name.str(), sec.getAddress(), sec.getSize());
    }
}

void provide_symbol(utils::ostream &s, const SymbolRef &gen_sym) {
    ELFSymbolRef sym(gen_sym);
    auto name = sym.getName();
    auto addr = sym.getAddress();
    if (!name) { utils::skip_symbol(s, name.getError()); return; }
    if (!addr) { utils::skip_symbol(s, addr.getError()); return; }
    utils::provide_symbol(s, name.get().str(), addr.get(), sym.getSize(),
                          (sym.getType() == SymbolRef::ST_Function));
}

template <typename T>
void provide_symbols(utils::ostream &s, const ELFObjectFile<T> &obj) {
    typedef typename ELFFile<T>::Elf_Shdr sec_hdr;
    std::size_t sym_count = std::distance(obj.symbol_begin(), obj.symbol_end());
    auto sections = obj.getELFFile()->sections();
    bool is_dyn = std::any_of(sections.begin(), sections.end(),
                              [](const sec_hdr &hdr) { return (hdr.sh_type == ELF::SHT_DYNSYM); });
    for (auto sym : obj.symbols())
        provide_symbol(s, sym);
//  due to a bug in llvm 3.8 with stripped static binaries, we have to perform such check
    if  (!sym_count && !is_dyn)
        return;
    for (auto sym : obj.getDynamicSymbolIterators())
        provide_symbol(s, sym);
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
void provide(utils::ostream &s, const T &t) {}

template <>
void provide(utils::ostream &s, const SectionRef &sec) {
    StringRef name;
    uint64_t addr, size;
    if (auto er = sec.getName(name))    { return s.fail(er.message()); }
    if (auto er = sec.getAddress(addr)) { return s.fail(er.message()); }
    if (auto er = sec.getSize(size))    { return s.fail(er.message()); }
    utils::provide_section(s, name.str(), addr, size);
}

template <>
void provide(utils::ostream &s, const SymbolRef &sym) {
    StringRef name;
    uint64_t addr, size;
    SymbolRef::Type kind;
    if (auto er = sym.getName(name))    { utils::skip_symbol(s, er); return; }
    if (auto er = sym.getAddress(addr)) { utils::skip_symbol(s, er); return; }
    if (auto er = sym.getSize(size))    { utils::skip_symbol(s, er); return; }
    if (auto er = sym.getType(kind))    { utils::skip_symbol(s, er); return; }
    utils::provide_symbol(s, name.str(), addr, size, (kind == SymbolRef::ST_Function));
}

template <typename T>
void iter(content_iterator<T> first, content_iterator<T> last, utils::ostream &s) {
    error_code er;
    while (first != last) {
        provide(s, *first);
        first.increment(er);
        if (er) {
            s.fail(er.message());
            return;
        }
    }
}

template <typename T>
void provide_symbols(utils::ostream &s, const ELFObjectFile<T> &obj) {
    iter(obj.begin_symbols(), obj.begin_symbols(), s);
    iter(obj.begin_dynamic_symbols(), obj.begin_dynamic_symbols(), s);
}

template <typename T>
void provide_sections(utils::ostream &s, const ELFObjectFile<T> &obj) {
    iter(obj.begin_sections(), obj.end_sections(), s);
}

#else
#error LLVM version is not supported
#endif

template <typename T>
void provide_segments(utils::ostream &s, const ELFObjectFile<T> &obj) {
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

} // namespace elf

template <typename T>
error_or<std::string> load(const ELFObjectFile<T> &obj) {
    utils::ostream s = success(std::ostringstream());
    auto arch_str = Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch()));
    *s << scheme::declare();
    *s << scheme::arch(arch_str);
    *s << scheme::entry_point(obj.getELFFile()->getHeader()->e_entry);
    elf::provide_segments(s, obj);
    elf::provide_sections(s, obj);
    if (!s) return failure(s.message());
    elf::provide_symbols(s, obj);
    if (!s) return failure(s.message());
    return std::move(success(s->str()) << s.warnings());
}

} // namespace loader

#endif // LLVM_ELF_LOADER_HPP
