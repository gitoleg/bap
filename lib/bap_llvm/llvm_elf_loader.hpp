#ifndef LLVM_ELF_LOADER_HPP
#define LLVM_ELF_LOADER_HPP

#include <iostream>
#include <sstream>

#include <llvm/Object/ELFObjectFile.h>

#include "llvm_error_or.hpp"


struct sexp {
    explicit sexp(std::string first) { s_ << std::boolalpha << "(" << first; }

    template <typename T>
    sexp & operator<<(const T &t) {
        s_ << " " << t; return *this;
    }

    std::string str() const { return s_.str() + ")"; }
    operator std::string() { return str(); }

    friend std::ostream &  operator<<(std::ostream &, const sexp &);

private:
    std::ostringstream s_;
};

std::ostream & operator<<(std::ostream &s, const sexp &exp) {
    return s << exp.str();
}

std::string quoted(const std::string &s) {
    if (s.size() == 0)
        return "\"" + s + "\"";
    else return s;
}

namespace loader {

using namespace llvm;
using namespace llvm::object;

static const std::string declarations =
    "(declare arch (name str))"
    "(declare entry-point (addr int))"
    "(declare program-header (entry-type int) (p-offset int) (v-addr int)"
     "(p-addr int) (p-size int) (v-size int) (flags int) (align int))"
    "(declare section-header (index int) (entry-type int) (flags int)"
     "(v-addr int) (p-offset int) (p-size int) (sh-link int) (sh-info int) (align int) (sh-entsize int))"
    "(declare symbol-entry (value int) (p-size int) (is-function bool) (name str))"
    "(declare string-table (sec-name str) (index int) (name str))";

template <typename T>
std::string file_header(const ELFFile<T> &obj) {
    auto hdr = obj.getHeader();
    return sexp("entry-point") << hdr->e_entry;
}

template <typename T>
std::string program_headers(const ELFFile<T> &obj) {
    std::ostringstream s;
    auto begin = obj.program_header_begin();
    auto end = obj.program_header_end();
    std::size_t n = 0;
    for (auto it = begin; it != end; ++it, ++n) {
        s << (sexp("program-header")
              << it->p_type << it->p_offset << it->p_vaddr << it->p_paddr
              << it->p_filesz << it->p_memsz << it->p_flags << it->p_align);
    }
    return s.str();
}

template <typename T, typename Sec>
std::string string_table(const ELFFile<T> &obj, const Sec *it) {
    std::ostringstream s;
    auto sec_name = obj.getSectionName(it);
    if (!sec_name) return "";
    auto tab = StringRef((const char *)obj.base() + it->sh_offset,  it->sh_size).str();
    uint64_t pos = 0, start = 0;
    std::string name = "";
    while (start < it->sh_size) {
        pos = tab.find('\0', start);
        if (pos == std::string::npos)
            break;
        name = tab.substr(start, pos - start);
        s << (sexp("string-table") << quoted(sec_name.get().str()) << start << quoted(name));
        start = pos + 1;
    }
    return s.str();
}

template <typename T>
std::string section_headers(const ELFFile<T> &obj) {
    std::ostringstream s;
    auto begin = obj.section_begin();
    auto end = obj.section_end();
    for (auto it = begin; it != end; ++it) {
        s << (sexp("section-header")
              << it->sh_name << it->sh_type << it->sh_flags << it->sh_addr
              << it->sh_offset << it->sh_size << it->sh_link << it->sh_info
              << it->sh_addralign << it->sh_entsize);
        if (it->sh_type == ELF::SHT_STRTAB)
            s << string_table(obj, it);
    }
    return s.str();
}


template <typename T>
std::string symbol_entries(const ELFFile<T> &obj) {
    std::ostringstream s;
    auto begin = obj.section_begin();
    auto end = obj.section_end();
    for (auto it = begin; it != end; ++it) {
        if (it->sh_type == ELF::SHT_SYMTAB) {
            auto sbegin = obj.symbol_begin(it);
            auto send = obj.symbol_end(it);
            for (auto sit = sbegin; sit != send; ++sit) {
                bool is_fun = sit->getType() == ELF::STT_FUNC;
                //auto name = symbol_name(obj, sit->st_name);
                //if (!name) continue;
                s << (sexp("symbol-entry") << sit->st_value << sit->st_size << is_fun << "test");
//                s << (sexp("symbol-entry") << addr.get() << size << is_fun << quoted(name.get().str()));
            }
        }
    }
    return s.str();
}

std::string arch(const ObjectFile& obj) {
    return (sexp("arch") <<
        Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch())));
}


//TODO: rework returning type .. possible
template <typename T>
error_or<std::string> load(const ELFObjectFile<T> &fobj) {
    ELFFile<T> obj = *(fobj.getELFFile());
    std::ostringstream s;
    s << declarations;
    s << arch(fobj);
    s << file_header(obj);
    s << program_headers(obj);
    s << section_headers(obj);
//    s << symbol_entries(obj);
    std::string res = s.str();

//    std::cout << res << std::endl << std::endl;

    return success(res);
}

} // namespace loader

#endif // LLVM_ELF_LOADER_HPP
