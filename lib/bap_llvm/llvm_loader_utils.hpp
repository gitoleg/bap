#ifndef LLVM_LOADER_UTILS_HPP
#define LLVM_LOADER_UTILS_HPP

#include <sstream>

#include "llvm_error_or.hpp"

namespace loader {

using namespace llvm;

struct data_stream {

    explicit data_stream() : s_(info()) {}

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
    error_or<info> s_;
};


std::string quoted(const std::string &s) {
    return "\"" + s + "\"";
}

} // namespace loader

#endif // LLVM_LOADER_UTILS_HPP
