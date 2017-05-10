#ifndef LLVM_COFF_LOADER_HPP
#define LLVM_COFF_LOADER_HPP

#include <llvm/ADT/Triple.h>
#include <llvm/Object/COFF.h>

#include "llvm_error_or.hpp"
#include "llvm_loader_utils.hpp"
#include <llvm/Object/SymbolSize.h>

namespace loader {

using namespace llvm;
using namespace llvm::object;

typedef COFFObjectFile coff_obj;

static const std::string coff_declarations =
    "(declare coff-format (flag bool))"
    "(declare arch (name str))"
    "(declare entry-point (addr int))"
    "(declare section-header (name str) (offset int) (size int))"
    "(declare virtual-section-header (name str) (addr int) (size int))"
    "(declare code-content (name str))"
    "(declare section-access (name str) (read bool) (write bool) (execute bool))"
    "(declare function (name str) (addr int) (size int))"
    ;


#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

void arch(const coff_obj &obj, data_stream &s) {
    s << "(arch " << Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch())) << ")";
}

//TODO: delete it, probably we don't need in it any more
void image_base(const coff_obj &obj, data_stream &s) {
    s << "(image-base " << obj.getImageBase() << ")";
}

void entry_point(const coff_obj &obj, data_stream &s) {
    if (obj.getBytesInAddress() == 4) {
        const pe32_header* hdr = 0;
        if (std::error_code ec = obj.getPE32Header(hdr)) { s.fail(ec.message()); return; }
        if (!hdr) { s.fail("PE header not found"); return; }
        s << "(entry-point " << hdr->AddressOfEntryPoint + hdr->ImageBase << ")";
    } else {
        const pe32plus_header *hdr = 0;
        if (std::error_code ec = obj.getPE32PlusHeader(hdr)) { s.fail(ec.message()); return; }
        if (!hdr) { s.fail("PE+ header not found"); return; }
        s << "(entry-point " << hdr->AddressOfEntryPoint + hdr->ImageBase << ")";
    }
}

void sections(const coff_obj &obj, data_stream &s) {
    for (auto sref : obj.sections()) {
        auto sec = obj.getCOFFSection(sref);
        bool r = static_cast<bool>(sec->Characteristics & COFF::IMAGE_SCN_MEM_READ);
        bool w = static_cast<bool>(sec->Characteristics & COFF::IMAGE_SCN_MEM_WRITE);
        bool x = static_cast<bool>(sec->Characteristics & COFF::IMAGE_SCN_MEM_EXECUTE);
        s << "(section-header " << sec->Name << " " << sec->PointerToRawData << " " << sec->SizeOfRawData << ")";
        s << "(virtual-section-header " << sec->Name << " " << sec->VirtualAddress << " "
          << sec->VirtualSize << ")";
        s << "(section-access " << sec->Name << " " << r << " " << w << " " << x << ")";
        if (sec->Characteristics & COFF::IMAGE_SCN_CNT_CODE)
            s << "(code-content " << sec->Name <<  ")";
    }
}

void symbols(const coff_obj &obj, data_stream &s) {
    auto syms = computeSymbolSizes(obj);

    for (auto sized_sym : syms) {
        auto sref = sized_sym.first;
        auto name = sref.getName();
        auto addr = sref.getAddress();
        if (!name || !addr) continue;
        if (sref.getType() == SymbolRef::ST_Function)
            s << "(function " << (*name).str() << " " << *addr << " " << sized_sym.second <<   ")";
    }
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 5

#else
#error LLVM version is not supported
#endif

error_or<std::string> load(const coff_obj &obj) {
    data_stream s;
    s << std::boolalpha << coff_declarations << "(coff-format true)";
    arch(obj,s);
    entry_point(obj, s);
    sections(obj, s);
    symbols(obj, s);
    return s.str();
}

} //namespace loader

#endif // LLVM_COFF_LOADER_HPP
