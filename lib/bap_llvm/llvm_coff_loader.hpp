#ifndef LLVM_COFF_LOADER_HPP
#define LLVM_COFF_LOADER_HPP

#include <sstream>

#include <llvm/Object/COFF.h>
#include <llvm/ADT/Triple.h>

#include "llvm_loader_scheme.hpp"
#include "llvm_error_or.hpp"

namespace coff_loader {

using namespace llvm;
using namespace llvm::object;

typedef error_or<std::ostringstream> ostream;

// why static casts ???
void provide_segment(ostream &s, const coff_section &sec, uint64_t image_base) {
    int offset = static_cast<int>(sec.PointerToRawData);
    uint64_t addr = static_cast<uint64_t>(sec.VirtualAddress + image_base);
    uint64_t size = static_cast<uint64_t>(sec.SizeOfRawData);
    bool r = static_cast<bool>(sec.Characteristics & COFF::IMAGE_SCN_MEM_READ);
    bool w = static_cast<bool>(sec.Characteristics & COFF::IMAGE_SCN_MEM_WRITE);
    bool x = static_cast<bool>(sec.Characteristics & COFF::IMAGE_SCN_MEM_EXECUTE);
    *s << scheme::segment(addr, size, r, w, x);
    *s << scheme::mapped(addr, size, offset);
    *s << scheme::named_region(addr, size, sec.Name);
}

void provide_section(ostream &s, const coff_section &sec, uint64_t image_base) {
    auto addr = sec.VirtualAddress + image_base;
    auto size = sec.SizeOfRawData;
    *s << scheme::section(addr, size) << scheme::named_region(addr, size, sec.Name);
}

void provide_symbol(ostream &s, const std::string &name, uint64_t addr, uint64_t size, bool is_fun) {
    *s << scheme::named_symbol(addr, name);
    if (is_fun) {
        *s << scheme::code_start(addr);
        *s << scheme::symbol_chunk(addr, size, addr);
    }
}

bool is_segment(const coff_section &s) {
    uint64_t c = static_cast<uint64_t>(s.Characteristics);
    return (c & COFF::IMAGE_SCN_CNT_CODE ||
            c & COFF::IMAGE_SCN_CNT_INITIALIZED_DATA ||
            c & COFF::IMAGE_SCN_CNT_UNINITIALIZED_DATA);
}

template <typename T>
void skip_symbol(ostream &s, const T &er) { s.warning() << "skipping symbol: " << er.message(); }

template <typename Syms>
void provide_symbols(ostream &s, const COFFObjectFile &obj, const Syms &symbols) {
    for (symbol_iterator it : symbols) {
        auto sym = obj.getCOFFSymbol(*it);
        const coff_section *sec = nullptr;

        if (sym.getSectionNumber() == COFF::IMAGE_SYM_UNDEFINED)
            continue;
        if (auto er = obj.getSection(sym.getSectionNumber(), sec)) {
            skip_symbol(s, er);
            continue;
        }
        if (!sec) continue;
        uint64_t size = sec->VirtualAddress + sec->SizeOfRawData - sym.getValue();

        for (symbol_iterator it : symbols) {
            auto next = obj.getCOFFSymbol(*it);
            if (next.getSectionNumber() == sym.getSectionNumber()) {
                auto new_size = next.getValue() > sym.getValue() ?
                    next.getValue() - sym.getValue() : size;
                size = new_size < size ? new_size : size;
            }
        }
        auto name = it->getName();
        auto addr = it->getAddress();
        if (!addr) { skip_symbol(s, addr.getError()); continue; }
        if (!name) { skip_symbol(s, addr.getError()); continue; }
        provide_symbol(s, name.get(), addr.get(), size,
                       (sym.getType() == SymbolRef::ST_Function));
    }
}

template <typename Secs>
void provide_sections(ostream &s, const COFFObjectFile &obj, const Secs &sections) {
    for (section_iterator it : sections)
        provide_section(s, *obj.getCOFFSection(*it), obj.getImageBase());
}


template <typename Segs>
void provide_segments(ostream &s, const COFFObjectFile& obj, const Segs &segments) {
    for (auto it : segments) {
        const coff_section *sec = obj.getCOFFSection(it);
        if (is_segment(*sec))
            provide_segment(s, *sec, obj.getImageBase());
    }
}


#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

void provide_sections(ostream &s, const COFFObjectFile &obj) {
    provide_segments(s, obj, obj.sections());
    provide_sections(s, obj, obj.sections());
}

error_or<uint64_t> image_entry(const COFFObjectFile& obj) {
    if (obj.getBytesInAddress() == 4) {
        const pe32_header* hdr = 0;
        if (auto ec = obj.getPE32Header(hdr))
            return failure(ec.message());
        if (!hdr) return failure("PE header not found");
        return error_or<uint64_t>(hdr->AddressOfEntryPoint + hdr->ImageBase);
    } else {
        const pe32plus_header *hdr = 0;
        if (auto ec = obj.getPE32PlusHeader(hdr))
            return failure(ec.message());
        if (!hdr) return failure("PE+ header not found");
        return error_or<uint64_t>(hdr->AddressOfEntryPoint + hdr->ImageBase);
    }
}

void provide_symbols(ostream &s, const COFFObjectFile &obj) {
    provide_symbols(s, obj, obj.symbols());
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

// #include "llvm/Support/system_error.h"
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

error_or<uint64_t> getImageBase(const COFFObjectFile &obj) {
    if (obj.getBytesInAddress() == 4) {
        const pe32_header *hdr;
        if (auto ec = obj.getPE32Header(hdr))
            return failure_of_error(ec);
        return error_or<uint64_t>(hdr->ImageBase);
    } else {
        error_or<pe32plus_header> hdr = getPE32PlusHeader(obj);
        if (!hdr) return hdr;
        return std::move(error_or<uint64_t>(hdr->ImageBase) << hdr.warnings());
    }
}

void provide_symbols(ostream &s, const COFFObjectFile& obj) {
    std::vector<symbol_iterator> syms;
    error_code ec;
    for (auto it = obj.begin_symbols(); it != obj.end_symbols(); it.increment(ec)) {
        if (ec) { return s.fail(ec.message()); return }
        syms.push_back(it);
    }
    provide_symbols(s, obj, syms);
}

void provide_sections(ostream &s, const COFFObjectFile& obj) {
    std::vector<symbol_iterator> secs;
    error_code ec;
    for (auto it = obj.begin_sections; it != obj.end_sectons(); it.increment(ec)) {
        if (ec) { return s.fail(ec.message()); return }
        secs.push_back(it);
    }
    provide_sections(s, obj, secs);
    provide_segments(s, obj, secs);
}

#else
#error LLVM version is not supported
#endif


error_or<std::string> load(const COFFObjectFile &obj) {
    ostream s = success(std::ostringstream());
    auto arch_str = Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch()));
    auto entry = image_entry(obj);
    if (!entry)
        return failure(entry.message());
    *s << scheme::declare();
    *s << scheme::arch(arch_str);
    *s << scheme::entry_point(*entry);
    provide_sections(s, obj);
    if (!s) return failure(s.message());
    provide_symbols(s, obj);
    if (!s) return failure(s.message());
    return std::move(success(s->str()) << s.warnings());
}

} // namespace coff_loader

#endif // LLVM_COFF_LOADER_HPP
