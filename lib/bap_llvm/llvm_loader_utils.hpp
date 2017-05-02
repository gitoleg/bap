#ifndef LLVM_LOADER_UTILS_HPP
#define LLVM_LOADER_UTILS_HPP

#include <sstream>
#include "llvm_error_or.hpp"

namespace loader {

struct sexp {
    explicit sexp(std::string first) { s_ << std::boolalpha << "(" << first; }

    template <typename T>
    sexp & operator<<(const T &t) {
        s_ << " " << t; return *this;
    }

    std::string str() const { return s_.str() + ")"; }
    operator std::string() { return str(); }

    friend std::ostream & operator<<(std::ostream &, const sexp &);

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

} // namespace loader

#endif // LLVM_LOADER_UTILS_HPP
