#ifndef LLVM_COFF_LOADER_HPP
#define LLVM_COFF_LOADER_HPP

#include <sstream>

#include <llvm/Object/COFF.h>
#include <llvm/ADT/Triple.h>

#include "llvm_loader_utils.hpp"
#include "llvm_error_or.hpp"


namespace loader {


using namespace llvm;
using namespace llvm::object;

namespace coff {

template <typename Syms>
void provide_symbols(utils::ostream &s, const COFFObjectFile &obj, const Syms &symbols);

template <typename Secs>
void provide_sections(utils::ostream &s, const COFFObjectFile &obj, const Secs &sections, uint64_t image_base);

template <typename Segs>
void provide_segments(utils::ostream &s, const COFFObjectFile& obj, const Segs &segments, uint64_t image_base);


#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

COFFSymbolRef get_symbol(const COFFObjectFile& obj, symbol_iterator it) {
    return obj.getCOFFSymbol(*it);
}

uint64_t symbol_value(const COFFSymbolRef &sym)   {
    return static_cast<uint64_t>(sym.getValue());
}

uint64_t section_number(const COFFSymbolRef &sym) {
    return static_cast<uint64_t>(sym.getSectionNumber());
}

error_or<std::string> get_name(const SymbolRef &s) {
    auto n = s.getName();
    if (!n) return failure(n.getError().message());
    else return success(n.get().str());
}

error_or<uint64_t> get_addr(const SymbolRef &s) {
    auto a = s.getAddress();
    if (!a) return failure(a.getError().message());
    else return success(a.get());
}

error_or<SymbolRef::Type> get_kind(const SymbolRef &s) {
    return success(s.getType());
}

void provide_sections(utils::ostream &s, const COFFObjectFile &obj) {
    provide_segments(s, obj, obj.sections(), obj.getImageBase());
    provide_sections(s, obj, obj.sections(), obj.getImageBase());
}

error_or<pe32plus_header> getPE32PlusHeader(const llvm::object::COFFObjectFile& obj) {
    const pe32plus_header *hdr = 0;
    if (auto ec = obj.getPE32PlusHeader(hdr))
        return failure(ec.message());
    else return success(hdr);
}

void provide_symbols(utils::ostream &s, const COFFObjectFile &obj) {
    provide_symbols(s, obj, obj.symbols());
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

const coff_symbol get_symbol(const COFFObjectFile& obj, symbol_iterator it) {
    return *obj.getCOFFSymbol(it);
}

uint64_t symbol_value(const coff_symbol &s) {
    return static_cast<uint64_t>(s.Value);
}

uint64_t section_number(const coff_symbol &s) {
    return static_cast<uint64_t>(s.SectionNumber);
}

error_or<std::string> get_name(const SymbolRef &s) {
    StringRef name;
    if(error_code ec = s.getName(name))
        return failure(ec.message());
    return success(name.str());
}

error_or<uint64_t> get_addr(const SymbolRef &s) {
    uint64_t addr;
    if (error_code ec = s.getAddress(addr))
        return failure(ec.message());
    return success(addr);
}
error_or<SymbolRef::Type> get_kind(const SymbolRef &s) {
    SymbolRef::Type kind;
    if (error_code ec = s.getType(kind))
        return failure(ec.message());
    return success(kind);
}

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
            return failure(ec.message());
        return error_or<uint64_t>(hdr->ImageBase);
    } else {
        error_or<pe32plus_header> hdr = getPE32PlusHeader(obj);
        if (!hdr) return hdr;
        return std::move(error_or<uint64_t>(hdr->ImageBase) << hdr.warnings());
    }
}

void provide_symbols(utils::ostream &s, const COFFObjectFile& obj) {
    std::vector<symbol_iterator> syms;
    error_code ec;
    for (auto it = obj.begin_symbols(); it != obj.end_symbols(); it.increment(ec)) {
        if (ec) { return s.fail(ec.message()); return; }
        syms.push_back(it);
    }
    provide_symbols(s, obj, syms);
}

void provide_sections(utils::ostream &s, const COFFObjectFile& obj) {
    std::vector<section_iterator> secs;
    error_code ec;
    for (auto it = obj.begin_sections(); it != obj.end_sections(); it.increment(ec)) {
        if (ec) { return s.fail(ec.message()); return; }
        secs.push_back(it);
    }
    auto base = getImageBase(obj);
    if (!base) { s.fail(base.message()); return; }
    provide_sections(s, obj, secs, *base);
    provide_segments(s, obj, secs, *base);
}

#else
#error LLVM version is not supported
#endif

bool is_segment(const coff_section &s) {
    uint64_t c = static_cast<uint64_t>(s.Characteristics);
    return (c & COFF::IMAGE_SCN_CNT_CODE ||
            c & COFF::IMAGE_SCN_CNT_INITIALIZED_DATA ||
            c & COFF::IMAGE_SCN_CNT_UNINITIALIZED_DATA);
}

template <typename Secs>
void provide_sections(utils::ostream &s, const COFFObjectFile &obj, const Secs &sections, uint64_t image_base) {
    for (auto it : sections) {
        auto sec = *obj.getCOFFSection(it);
        utils::provide_section(s, sec.Name, sec.VirtualAddress + image_base, sec.SizeOfRawData);
    }
}

template <typename Segs>
void provide_segments(utils::ostream &s, const COFFObjectFile& obj, const Segs &segments, uint64_t image_base) {
    for (auto it : segments) {
        const coff_section *sec = obj.getCOFFSection(it);
        if (is_segment(*sec)) {
            int offset = static_cast<int>(sec->PointerToRawData);
            uint64_t addr = static_cast<uint64_t>(sec->VirtualAddress + image_base);
            uint64_t size = static_cast<uint64_t>(sec->SizeOfRawData);
            bool r = static_cast<bool>(sec->Characteristics & COFF::IMAGE_SCN_MEM_READ);
            bool w = static_cast<bool>(sec->Characteristics & COFF::IMAGE_SCN_MEM_WRITE);
            bool x = static_cast<bool>(sec->Characteristics & COFF::IMAGE_SCN_MEM_EXECUTE);
            *s << scheme::segment(addr, size, r, w, x);
            *s << scheme::mapped(addr, size, offset);
            *s << scheme::named_region(addr, size, sec->Name);
        }
    }
}

//TODO: why don't just computeSymbols in 3.8 ?
//TODO: check size inference
template <typename Syms>
void provide_symbols(utils::ostream &s, const COFFObjectFile &obj, const Syms &symbols) {
    for (symbol_iterator it : symbols) {
        auto sym = get_symbol(obj, it);
        const coff_section *sec = nullptr;
        auto sec_num = section_number(sym);

        if (sec_num == COFF::IMAGE_SYM_UNDEFINED)
            continue;
        if (auto er = obj.getSection(sec_num, sec)) {
            utils::skip_symbol(s, er);
            continue;
        }
        if (!sec) continue;
        auto sym_val = symbol_value(sym);
        uint64_t size = sec->VirtualAddress + sec->SizeOfRawData - sym_val;

        for (symbol_iterator it : symbols) {
            auto next = get_symbol(obj, it);
            auto next_sec_num = section_number(next);
            auto next_sym_val = symbol_value(next);
            if (next_sec_num == sec_num ) {
                auto new_size = next_sym_val > sym_val ? next_sym_val - sym_val : size;
                size = new_size < size ? new_size : size;
            }
        }
        auto name = get_name(*it);
        auto addr = get_addr(*it);
        auto kind = get_kind(*it);
        if (!addr) { utils::skip_symbol(s, name); continue; }
        if (!name) { utils::skip_symbol(s, addr); continue; }
        if (!kind) { utils::skip_symbol(s, kind); continue; }
        utils::provide_symbol(s, *name, *addr, size, (*kind == SymbolRef::ST_Function));
    }
}

error_or<uint64_t> image_entry(const COFFObjectFile& obj) {
    if (obj.getBytesInAddress() == 4) {
        const pe32_header* hdr = 0;
        if (auto ec = obj.getPE32Header(hdr))
            return failure(ec.message());
        if (!hdr) return failure("PE header not found");
        return error_or<uint64_t>(hdr->AddressOfEntryPoint + hdr->ImageBase);
    } else {
        auto hdr = getPE32PlusHeader(obj);
        if (!hdr) return failure("PE+ header not found");
        return error_or<uint64_t>(hdr->AddressOfEntryPoint + hdr->ImageBase);
    }
}

} // namespace coff

error_or<std::string> load(const COFFObjectFile &obj) {
    utils::ostream s = success(std::ostringstream());
    auto arch_str = Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch()));
    auto entry = coff::image_entry(obj);
    if (!entry)
        return failure(entry.message());
    *s << scheme::declare();
    *s << scheme::arch(arch_str);
    *s << scheme::entry_point(*entry);
    coff::provide_sections(s, obj);
    if (!s) return failure(s.message());
    coff::provide_symbols(s, obj);
    if (!s) return failure(s.message());
    return std::move(success(s->str()) << s.warnings());
}

} // namespace loader

#endif // LLVM_COFF_LOADER_HPP
