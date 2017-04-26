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

error_or<std::string> unsupported() { return error_or(""); }
bool is_supported(const error_or<std::string> &loaded) {
    return (loaded && loaded->get().size() != 0)
}

template <typename T>
error_or<std::string> load_elf(const object::Binary *binary) {
    if (auto bin = cast<T>(binary))
        return elf_loader::load(*bin);
    else
        return unsupported; //("Unrecognized ELF format");
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
    else return unsupported; //failure("Unrecognized ELF format");
}

error_or<std::string> load_coff(const object::Binary *binary) {
    return unsupported(); // "COFF is not supported in ogre loader"
}

error_or<std::string> load_macho(const object::Binary *binary) {
    return unsupported(); // "Macho is not supported in ogre loader"
}

typedef error_or<std::string> bap_llvm_loader;

bap_llvm_loader * make_loader(const error_or<std::string> &loaded) {
    if (loaded && is_supported(loaded))
        return new bap_llvm_loader(loaded);
    else if (!loaded)
        return new bap_llvm_loader(loaded);
    else
        return nullptr;
}

const bap_llvm_loader * create(const char* data, std::size_t size) {
    error_or<object::Binary> bin = get_binary(data, size);
    bap_llvm_loader *loader;
    if (!bin) {
        loader = make_loader(failure("Bad file format"));
    }
    else {
        if (bin->isCOFF())
            loader = make_loader(load_coff(bin.get()));
        else if (bin->isELF())
            loader = make_loader(load_elf(bin.get()));
        else if (bin->isMachO())
            loader = make_loaded(load_macho(bin.get()));
        else
            loader = make_loader(failure("Unrecognized object format"));
    }
    return loader;
}

void destroy(const bap_llvm_loader *loader) {
    if (loader)
        delete loader;
}

} // namespace loader

#endif // LLVM_LOADER_HPP
