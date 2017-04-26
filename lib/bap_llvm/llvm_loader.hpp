#ifndef LLVM_LOADER_HPP
#define LLVM_LOADER_HPP

#include "llvm_elf_loader.hpp"
#include "llvm_loader_scheme.hpp"

namespace loader {

using namespace llvm;
using namespace llvm::object;


#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

error_or<object::Binary> get_binary(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBufferRef buf(data_ref, "binary");
    auto binary = createBinary(buf);
    if (auto ec = binary.getError())
        return failure(ec.message());
    error_or<object::Binary> v(binary->release());
    return v;
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4
error_or<object::Binary> get_binary(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBuffer* buff(MemoryBuffer::getMemBufferCopy(data_ref, "binary"));
    OwningPtr<object::Binary> bin;
    if (error_code ec = createBinary(buff, bin))
        return failure(ec.message());
    return error_or<object::Binary>(bin.take());
}

#else
#error LLVM version is not supported
#endif

template <typename T>
const T* cast(const object::Binary *binary) {
    return llvm::dyn_cast<T>(binary);
}

error_or<std::string> unsupported_filetype() { return success(""); }

template <typename T>
error_or<std::string> load_elf(const object::Binary *binary) {
    if (auto bin = cast<T>(binary))
        return elf_loader::load(*bin);
    else
        return unsupported_filetype();
}

error_or<std::string> load_elf(const object::Binary *binary) {
    if (isa<ELF32LEObjectFile>(*binary))
        return load_elf<ELF32LEObjectFile>(binary);
    else if (isa<ELF32BEObjectFile>(*binary))
        return load_elf<ELF32BEObjectFile>(binary);
    else if (isa<ELF64LEObjectFile>(*binary))
        return load_elf<ELF64LEObjectFile>(binary);
    else if (isa<ELF64BEObjectFile>(*binary))
        return load_elf<ELF64BEObjectFile>(binary);
    else return unsupported_filetype();
}

void verbose_fails(const error_or<std::string> &loaded) {
    if(const char* env_p = std::getenv("BAP_DEBUG")) {
        if (std::string(env_p) == "1")
            if (!loaded)
                std::cerr << "ogre llvm loader error: " << loaded.message() << std::endl;
            for (auto w : loaded.warnings())
                std::cerr << "ogre llvm loader warning:  " << w << std::endl;
    }
}

error_or<std::string> load_coff(const object::Binary *binary) {
    return unsupported_filetype();
}

error_or<std::string> load_macho(const object::Binary *binary) {
    return unsupported_filetype();
}

error_or<std::string> load(const char* data, std::size_t size) {
    error_or<object::Binary> bin = get_binary(data, size);
    if (!bin)
        return unsupported_filetype();
    else if (bin->isCOFF())
        return load_coff(bin.get());
    else if (bin->isELF())
        return load_elf(bin.get());
    else if (bin->isMachO())
        return load_macho(bin.get());
    else
        return unsupported_filetype();
}

typedef error_or<std::string> bap_llvm_loader;

const bap_llvm_loader * create(const char* data, std::size_t size) {
    auto loaded = load(data, size);
    verbose_fails(loaded);
    return new bap_llvm_loader(std::move(loaded));
}

bool loader_failed(const bap_llvm_loader * loader) {
    return loader->has_error();
}

bool file_not_supported(const bap_llvm_loader * loader) {
    return (!loader->has_error() && (*loader)->size() == 0);
}

void destroy(const bap_llvm_loader *loader) {
    delete loader;
}

} // namespace loader

#endif // LLVM_LOADER_HPP
