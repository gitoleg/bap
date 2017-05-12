#ifndef LLVM_LOADER_UTILS_HPP
#define LLVM_LOADER_UTILS_HPP

#include <sstream>

#include <llvm/ADT/Triple.h>

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

std::string arch_of_object(const llvm::object::ObjectFile &obj) {
    return Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch()));
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

#include <llvm/Object/ObjectFile.h>

using namespace llvm::object;

template <typename T>
void next(content_iterator<T> &it, content_iterator<T> end) {
    error_code ec;
    it.increment(ec);
    if (ec) it = end;
}

#endif

} // namespace loader

#endif // LLVM_LOADER_UTILS_HPP
