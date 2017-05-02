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

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

template <typename T>
const typename ELFFile<T>::Elf_Phdr* pheader_begin(const ELFFile<T> &elf) {
    return elf.program_header_begin();
}

template <typename T>
const typename ELFFile<T>::Elf_Phdr* pheader_end(const ELFFile<T> &elf) {
    return elf.program_header_end();
}

template <typename T>
const typename ELFFile<T>::Elf_Shdr* begin_sections(const ELFFile<T> &elf) {
    return elf.section_begin();
}

template <typename T>
const typename ELFFile<T>::Elf_Shdr* end_sections(const ELFFile<T> &elf) {
    return elf.section_end();
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

template <typename T>
const typename ELFFile<T>::Elf_Phdr_Iter pheader_begin(const ELFFile<T> &elf) {
    return elf.begin_program_headers();
}

template <typename T>
const typename ELFFile<T>::Elf_Phdr_Iter pheader_end(const ELFFile<T> &elf) {
    return elf.end_program_headers();
}

template <typename T>
const typename ELFFile<T>::Elf_Shdr_Iter begin_sections(const ELFFile<T> &elf) {
    return elf.begin_sections();
}

template <typename T>
const typename ELFFile<T>::Elf_Shdr_Iter end_sections(const ELFFile<T> &elf) {
    return elf.end_sections();
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

template <typename T>
std::string file_header(const ELFFile<T> &elf) {
    auto hdr = elf.getHeader();
    return sexp("entry-point") << hdr->e_entry;
}

template <typename T>
std::string program_headers(const ELFFile<T> &elf) {
    std::ostringstream s;
    auto begin = pheader_begin(elf);
    auto end = pheader_end(elf);
    for (auto it = begin; it != end; ++it) {
        bool ld = (it->p_type == ELF::PT_LOAD);
        bool r = static_cast<bool>(it->p_flags & ELF::PF_R);
        bool w = static_cast<bool>(it->p_flags & ELF::PF_W);
        bool x = static_cast<bool>(it->p_flags & ELF::PF_X);
        s << (sexp("program-header")  << it->p_offset << it->p_filesz);
        s << (sexp("virtual-pheader") << it->p_offset << it->p_filesz << it->p_vaddr << it->p_memsz);
        s << (sexp("pheader-flags")   << it->p_offset << it->p_filesz << ld << r << w << x);
    }
    return s.str();
}

template <typename T>
std::string section_headers(const ELFFile<T> &elf) {
    std::ostringstream s;
    auto begin = begin_sections(elf);
    auto end = end_sections(elf);
    for (auto it = begin; it != end; ++it) {
        if (auto name = elf.getSectionName(it))
            s << (sexp("section-header") << quoted(name.get().str()) << it->sh_addr << it->sh_size);
    }
    return s.str();
}

// template <typename T>
// std::string symbol_entries(const ELFFile<T> &obj) {
//     std::ostringstream s;
//     auto begin = obj.section_begin();
//     auto end = obj.section_end();
//     for (auto it = begin; it != end; ++it) {
//         if (it->sh_type == ELF::SHT_SYMTAB) {
//             auto sbegin = obj.symbol_begin(it);
//             auto send = obj.symbol_end(it);
//             for (auto sit = sbegin; sit != send; ++sit) {
//                 bool is_fun = sit->getType() == ELF::STT_FUNC;
//                 //auto name = symbol_name(obj, sit->st_name);
//                 //if (!name) continue;
//                 s << (sexp("symbol-entry") << sit->st_value << sit->st_size << is_fun << "test");
// //                s << (sexp("symbol-entry") << addr.get() << size << is_fun << quoted(name.get().str()));
//             }
//         }
//     }
//     return s.str();
// }

std::string arch(const ObjectFile& obj) {
    return (sexp("arch") <<
        Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch())));
}


//TODO: rework returning type .. possible
template <typename T>
error_or<std::string> load(const ELFObjectFile<T> &obj) {
    ELFFile<T> elf = *(obj.getELFFile());
    std::ostringstream s;
    s << declarations;
    s << arch(obj);
    s << file_header(elf);
    s << program_headers(elf);
    s << section_headers(elf);
//    s << symbol_entries(elf);
    std::string res = s.str();

    std::cout << res << std::endl << std::endl;

    return success(res);
}

} // namespace loader

#endif // LLVM_ELF_LOADER_HPP
