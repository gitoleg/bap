#ifndef LLVM_LOADER_UTILS_HPP
#define LLVM_LOADER_UTILS_HPP

#include <sstream>
#include "llvm_error_or.hpp"

namespace loader {

namespace scheme {

struct sexp {
    explicit sexp(std::string first) { s_ << std::boolalpha << "(" << first; }

    template <typename T>
    sexp & operator<<(const T &t) { s_ << " " << t; return *this; }

    std::string str() const { return s_.str() + ")"; }
    operator std::string() { return str(); }

private:
    std::ostringstream s_;
};

std::string quoted(const std::string &s) {
    return "\"" + s + "\"";
}

std::string declare() {
    return
"(declare arch (name str))(declare code-start (addr int))(declare entry-point (addr int))\
(declare mapped (addr int) (size int) (off int))(declare named-region (addr int) (size int) (name str))\
(declare named-symbol (addr int) (name str))(declare section (addr int) (size int))\
(declare segment (addr int) (size int) (r bool) (w bool) (x bool))\
(declare symbol-chunk (addr int) (size int) (root int))";
}

std::string arch(std::string arch) { return sexp("arch") << arch; }
std::string entry_point(uint64_t entry) { return sexp("entry-point") << entry; }
std::string code_start(uint64_t addr) { return sexp("code-start") << addr; }
std::string segment(uint64_t addr, uint64_t size, bool r, bool w, bool x) {
    return sexp("segment") << addr << size << r << w << x;
}

std::string mapped(uint64_t addr, uint64_t size, int off) {
    return sexp("mapped") << addr << size << off;
}

std::string section(uint64_t addr, uint64_t size) {
    return sexp("section") << addr << size;
}

std::string named_region(uint64_t addr, uint64_t size, std::string name) {
    return sexp("named-region") << addr << size << quoted(name);
}

std::string named_symbol(uint64_t addr, std::string name) {
    return sexp("named-symbol") << addr << quoted(name);
}

std::string symbol_chunk(uint64_t addr, uint64_t size, uint64_t root) {
    return sexp("symbol-chunk") << addr << size << root;
}
} // namespace scheme


namespace utils {

typedef error_or<std::ostringstream> ostream;

template <typename T>
void skip_symbol(ostream &s, const T &er) {
    s.warning() << "skipping symbol: " << er.message();
}

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


} // namespace utils
} // namespace loader

#endif // LLVM_LOADER_UTILS_HPP
