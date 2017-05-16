#ifndef LLVM_MACHO_LOADER_HPP
#define LLVM_MACHO_LOADER_HPP

#include <iostream>
#include <iomanip>

#include <llvm/Support/MachO.h>
#include <llvm/Object/MachO.h>

#include "llvm_error_or.hpp"
#include "llvm_loader_utils.hpp"

namespace loader {
namespace macho_loader {

using namespace llvm;
using namespace llvm::object;

typedef llvm::object::MachOObjectFile macho;
typedef macho::LoadCommandInfo command_info;

// check that symbol belongs to some sections,
// i.e. it's type is N_SECT.
bool is_in_section(const macho &obj, SymbolRef sym) {
    auto raw = sym.getRawDataRefImpl();
    const char *p = reinterpret_cast<const char *>(raw.p);
    if (p < obj.getData().begin() ||
        p + sizeof(MachO::nlist) > obj.getData().end())
        return false;
    const MachO::nlist *entry =
        reinterpret_cast<const MachO::nlist *>(raw.p);
    return ((entry->n_type & MachO::N_TYPE) == MachO::N_SECT);
}

static std::string macho_declarations =
    "(declare file-type (name str))"
    "(declare arch (name str))"
    "(declare entry-point (addr int))"
    "(declare segment-command (name str) (offset int) (size int))"
    "(declare segment-command-flags (name str) (read bool) (write bool) (execute bool))"
    "(declare segment-command-mapping (name str) (addr int) (size int))"
    "(declare section (name str) (offset int) (size int))"
    "(declare symbol (name str) (addr int) (size int))"
    "(declare function (addr int))";

template <typename T>
void segment_command(const T &cmd, data_stream &s) {
    bool r = static_cast<bool>(cmd.initprot & MachO::VM_PROT_READ);
    bool w = static_cast<bool>(cmd.initprot & MachO::VM_PROT_WRITE);
    bool x = static_cast<bool>(cmd.initprot & MachO::VM_PROT_EXECUTE);
    s << "(segment-command " << cmd.segname << " " << cmd.fileoff << " " << cmd.filesize << ")";
    s << "(segment-command-flags " << cmd.segname << " " << r << " " << w << " " << x << ")";
    s << "(segment-command-mapping " << cmd.segname << " " << cmd.vmaddr << " " << cmd.vmsize << ")";
}

void entry(command_info &info, data_stream &s) {
    const MachO::entry_point_command *entry_cmd =
        reinterpret_cast<const MachO::entry_point_command*>(info.Ptr);
    s << "(entry-point " << entry_cmd->entryoff << ")";
}

void macho_command(const macho &obj, command_info &info, data_stream &s) {
    if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT_64)
        segment_command(obj.getSegment64LoadCommand(info), s);
    if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT)
        segment_command(obj.getSegmentLoadCommand(info), s);
    if (info.C.cmd == MachO::LoadCommandType::LC_MAIN)
        entry(info, s);
}

template <typename S>
void section(const S & sec, data_stream &s) {
    s << "(section " << sec.sectname << " " << sec.offset << " " << sec.size << ")";
}

void symbol(const std::string &name, uint64_t addr, uint64_t size, SymbolRef::Type typ, data_stream &s) {
    s << "(symbol " << quoted(name) << " " << addr << " " << size << ")";
    if (typ == SymbolRef::ST_Function)
        s << "(function " << addr << ")";
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

void macho_commands(const macho &obj, data_stream &s) {
    for (auto it : obj.load_commands())
        macho_command(obj, it, s);
}

void sections(const macho &obj, data_stream &s) {
    for (auto sec : obj.sections())
        section(obj.getSection(sec.getRawDataRefImpl()), s);
}

void symbols(const macho &obj, data_stream &s) {
    auto sizes = computeSymbolSizes(obj);
    for (auto sized_sym : sizes) {
        auto sym = sized_sym.first;
        if (!is_in_section(obj, sym)) continue;
        auto size = sized_sym.second;
        auto addr = sym.getAddress();
        auto name = sym.getName();
        if (!addr || !name) continue;
        auto typ = sym.getType();
        symbol((*name).str(), *addr, size, typ, s);
    }
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

void macho_commands(const macho &obj, data_stream &s) {
    std::size_t cmd_count = 0;
    if (obj.is64Bit())
        cmd_count = obj.getHeader64().ncmds;
    else
        cmd_count = obj.getHeader().ncmds;
    command_info info = obj.getFirstLoadCommandInfo();
    for (std::size_t i = 0; i < cmd_count; ++i)
        macho_command(obj, info, s);
}

void sections(const macho &obj, data_stream &s) {
    auto end = obj.end_sections();
    for (auto it = obj.begin_sections(); it != end; next(it, end))
        section(obj.getSection(it->getRawDataRefImpl()), s);
}

void symbols(const macho &obj, data_stream &s) {
    StringRef name;
    uint64_t addr, size;
    SymbolRef::Type typ;
    auto end = obj.end_symbols();
    for (auto it = obj.begin_symbols(); it != end; next(it, end)) {
        auto er_name = it->getName(name);
        auto er_addr = it->getAddress(addr);
        auto er_size = it->getSize(size);
        auto er_type = it->getType(typ);
        if (er_name || er_addr || er_size || er_type) continue;
        if (is_in_section(obj, *it))
            symbol(name.str(), addr, size, *typ, s);
    }
}

#else
#error LLVM version is not supported
#endif

} // namespace macho_loader

error_or<std::string> load(const llvm::object::MachOObjectFile &obj) {
    using namespace macho_loader;
    data_stream s;
    s << std::boolalpha << macho_declarations;
    s << "(file-type macho)";
    s << "(arch " << arch_of_object(obj) << ")";
    macho_commands(obj, s);
    sections(obj, s);
    symbols(obj, s);
    return s.str();
}

} // namespace loader

#endif //  LLVM_MACHO_LOADER_HPP
