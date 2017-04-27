#ifndef LLVM_LOADER_SCHEME_HPP
#define LLVM_LOADER_SCHEME_HPP

#include <sstream>

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

std::string nonempty(const std::string &s) {
    if (s == "") return "\"\"";
    else return s;
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
    return sexp("named-region") << addr << size << nonempty(name);
}

std::string named_symbol(uint64_t addr, std::string name) {
    return sexp("named-symbol") << addr << nonempty(name);
}

std::string symbol_chunk(uint64_t addr, uint64_t size, uint64_t root) {
    return sexp("symbol-chunk") << addr << size << root;
}

} // namespace scheme

#endif // LLVM_LOADER_SCHEME_HPP
