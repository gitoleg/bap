#ifndef LLVM_COFF_LOADER_HPP
#define LLVM_COFF_LOADER_HPP

#include <tuple>

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
    "(declare symbol (name str) (addr int) (size int))"
    "(declare function (addr int) (size int))";

void arch(const coff_obj &obj, data_stream &s) {
    s << "(arch " << Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch())) << ")";
}

void section(const coff_section &sec, uint64_t image_base,  data_stream &s) {
    bool r = static_cast<bool>(sec.Characteristics & COFF::IMAGE_SCN_MEM_READ);
    bool w = static_cast<bool>(sec.Characteristics & COFF::IMAGE_SCN_MEM_WRITE);
    bool x = static_cast<bool>(sec.Characteristics & COFF::IMAGE_SCN_MEM_EXECUTE);
    s << "(section-header " << sec.Name << " " << sec.PointerToRawData << " " << sec.SizeOfRawData << ")";
    s << "(virtual-section-header " << sec.Name << " " << sec.VirtualAddress + image_base << " "
      << sec.VirtualSize << ")";
    s << "(section-access " << sec.Name << " " << r << " " << w << " " << x << ")";
    //TODO: ask here
    if (sec.Characteristics & COFF::IMAGE_SCN_CNT_CODE)
        s << "(code-content " << sec.Name <<  ")";
}

error_or<pe32plus_header> getPE32PlusHeader(const llvm::object::COFFObjectFile& obj);

void entry_point(const coff_obj &obj, data_stream &s) {
    if (obj.getBytesInAddress() == 4) {
        const pe32_header* hdr = 0;
        if (auto ec = obj.getPE32Header(hdr)) { s.fail(ec.message()); return; }
        if (!hdr) { s.fail("PE header not found"); return; }
        s << "(entry-point " << hdr->AddressOfEntryPoint + hdr->ImageBase << ")";
    } else {
        error_or<pe32plus_header> hdr = getPE32PlusHeader(obj);
        if (!hdr) { s.fail("PE+ header not found"); return; }
        s << "(entry-point " << hdr->AddressOfEntryPoint + hdr->ImageBase << ")";
    }
}

// SymbolRef, symbol value, section where is defined
typedef std::tuple<SymbolRef, uint64_t, const coff_section*> coff_sym_info;
typedef std::vector<std::pair<SymbolRef, uint64_t>> symbol_sizes;

symbol_sizes get_symbols_sizes(const std::vector<coff_sym_info> &syms) {
    symbol_sizes sizes;
    for (auto s : syms) {
        auto ref = std::get<0>(s);
        auto val = std::get<1>(s);
        auto sec = std::get<2>(s);
        uint64_t size = sec->VirtualAddress + sec->SizeOfRawData - val;
        for (auto next : syms) {
            auto next_val = std::get<1>(next);
            auto next_sec = std::get<2>(next);
            if (sec->PointerToRawData == next_sec->PointerToRawData) {
                auto new_size = next_val > val ? next_val - val : size;
                size = new_size < size ? new_size : size;
            }
        }
        sizes.push_back(std::make_pair(ref, size));
    }
    return sizes;
}

const coff_section * get_section(const COFFObjectFile& obj, std::size_t index) {
    const coff_section *sec = nullptr;
    bool fail = (index == COFF::IMAGE_SYM_UNDEFINED) || obj.getSection(index, sec);
    if (fail) return nullptr;
    else return sec;
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

error_or<pe32plus_header> getPE32PlusHeader(const llvm::object::COFFObjectFile& obj) {
    const pe32plus_header *hdr = 0;
    auto ec = obj.getPE32PlusHeader(hdr);
    if (ec) return failure(ec.message());
    else if (!hdr) { return failure("PE header not found"); }
    else return success(*hdr);
}

void sections(const coff_obj &obj, data_stream &s) {
    auto base = obj.getImageBase();
    for (auto sref : obj.sections())
        section(*obj.getCOFFSection(sref), base, s);
}

typedef std::vector<std::pair<SymbolRef, uint64_t>> symbol_sizes;

// We shall not use! computeSymbolSizes function for coff
// files, because it doesn't take in account some unhappy
// outcomes, so it's possible to get accidently a symbol's
// size eqauls to 18446744073709550526.
symbol_sizes getSymbolSizes(const COFFObjectFile& obj) {
    std::vector<coff_sym_info> info;
    for (symbol_iterator it : obj.symbols()) {
        auto sym = obj.getCOFFSymbol(*it);
        const coff_section *sec = get_section(obj, sym.getSectionNumber());
        if (!sec) continue;
        info.push_back(std::make_tuple(*it, sym.getValue(), sec));
    }
    return get_symbols_sizes(info);
}

void symbols(const coff_obj &obj, data_stream &s) {
    auto syms = getSymbolSizes(obj);
    for (auto sized_sym : syms) {
        auto sref = sized_sym.first;
        auto name = sref.getName();
        auto addr = sref.getAddress();
        if (!name || !addr) continue;
        s << "(symbol " << (*name).str() << " " << *addr << " " << sized_sym.second << ")";
        if (sref.getType() == SymbolRef::ST_Function)
            s << "(function " << *addr << " " << sized_sym.second << ")";
    }
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

error_or<pe32plus_header> getPE32PlusHeader(const llvm::object::COFFObjectFile& obj) {
    uint64_t cur_ptr = 0;
    const char *buf = (obj.getData()).data();
    const uint8_t *start = reinterpret_cast<const uint8_t *>(buf);
    uint8_t b0 = start[0];
    uint8_t b1 = start[1];
    if (b0 == 0x4d && b1 == 0x5a) { // check if this is a PE/COFF file
        // a pointer at offset 0x3C points to the
        cur_ptr += *reinterpret_cast<const uint16_t *>(start + 0x3c);
        // check the PE magic bytes.
        if (std::memcmp(start + cur_ptr, "PE\0\0", 4) != 0)
            return failure("PE Plus header not found");
        cur_ptr += 4; // skip the PE magic bytes.
        cur_ptr += sizeof(llvm::object::coff_file_header);
        auto p = reinterpret_cast<const pe32plus_header *>(start + cur_ptr);
        return error_or<pe32plus_header>(*p);
    }
    return failure("Failed to extract PE32+ header");
}

template <typename I>
void next(I &it, I end) {
    error_code ec;
    it.increment(ec);
    if (ec) it = end;
}
error_or<uint64_t> getImageBase(const COFFObjectFile &obj) {
    if (obj.getBytesInAddress() == 4) {
        const pe32_header *hdr;
        if (error_code ec = obj.getPE32Header(hdr))
            return failure(ec.message());
        return error_or<uint64_t>(hdr->ImageBase);
    } else {
        error_or<pe32plus_header> hdr = getPE32PlusHeader(obj);
        if (!hdr) return hdr;
        return std::move(error_or<uint64_t>(hdr->ImageBase) << hdr.warnings());
    }
}

void sections(const coff_obj &obj, data_stream &s) {
    auto base = getImageBase(obj);
    if (!base) { s.fail(base.message()); return; }
    auto end = obj.end_sections();
    for (auto it = obj.begin_sections(); it != end; next(it, end))
        section(*obj.getCOFFSection(*it), *base, s);
}

symbol_sizes getSymbolSizes(const COFFObjectFile& obj) {
    std::vector<coff_sym_info> info;
    for (symbol_iterator it : obj.symbols()) {
        auto sym = obj.getCOFFSymbol(*it);
        const coff_section *sec = get_section(obj, sym->SectionNumber());
        if (!sec) continue;
        info.push_back(std::make_tuple(*it, sym->Value(), sec));
    }
    return get_symbols_sizes(info);
}


void symbols(const coff_obj &obj, data_stream &s) {
// TODO
}

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
