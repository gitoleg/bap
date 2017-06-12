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
typedef SymbolRef::Type sym_type;

const MachO::nlist * sym_entry(const macho &obj, SymbolRef sym) {
    auto raw = sym.getRawDataRefImpl();
    const char *p = reinterpret_cast<const char *>(raw.p);
    if (p < obj.getData().begin() ||
        p + sizeof(MachO::nlist) > obj.getData().end())
        return nullptr;
    return reinterpret_cast<const MachO::nlist *>(raw.p);
}

// check that symbol belongs to some sections,
// i.e. it's type is N_SECT.
bool is_in_section(const MachO::nlist *entry) {
    if (!entry) return false;
    else return ((entry->n_type & MachO::N_TYPE) == MachO::N_SECT);
}

static std::string macho_declarations =
    "(declare file-type (name str))"
    "(declare arch (name str))"
    "(declare entry-point (addr int))"
    "(declare segment-command (name str) (offset int) (size int))"
    "(declare segment-command-flags (name str) (read bool) (write bool) (execute bool))"
    "(declare virtual-segment-command (name str) (addr int) (size int))"
    "(declare macho-section (name str) (addr int) (size int))"
    "(declare macho-section-symbol (name str) (addr int) (size int))"
    "(declare macho-symbol (name str) (value int))"
    "(declare function (addr int))";

template <typename T>
void segment_command(const T &cmd, ogre_doc &s) {
    bool r = static_cast<bool>(cmd.initprot & MachO::VM_PROT_READ);
    bool w = static_cast<bool>(cmd.initprot & MachO::VM_PROT_WRITE);
    bool x = static_cast<bool>(cmd.initprot & MachO::VM_PROT_EXECUTE);
    s.entry("segment-command") << cmd.segname << cmd.fileoff << cmd.filesize;
    s.entry("segment-command-flags") << cmd.segname << r << w << x;
    s.entry("virtual-segment-command") << cmd.segname << cmd.vmaddr << cmd.vmsize;
}

void entry_point(command_info &info, ogre_doc &s) {
    const MachO::entry_point_command *entry_cmd =
        reinterpret_cast<const MachO::entry_point_command*>(info.Ptr);
    s.entry("entry-point") << entry_cmd->entryoff;
}

void macho_command(const macho &obj, command_info &info, ogre_doc &s) {
    if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT_64)
        segment_command(obj.getSegment64LoadCommand(info), s);
    if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT)
        segment_command(obj.getSegmentLoadCommand(info), s);
    if (info.C.cmd == MachO::LoadCommandType::LC_MAIN)
        entry_point(info, s);
}

template <typename S>
void section(const S & sec, ogre_doc &s) {
    s.entry("macho-section") << sec.sectname << sec.addr << sec.size;
}

// we distinguish symbols that are defined in some section and symbols that are not. For former it's ok
// to provide size and interpret symbol's value as an address. For later we provide only name and value
// as it is.
void section_symbol(const std::string &name, uint64_t addr, uint64_t size, sym_type typ, ogre_doc &s) {
    s.entry("macho-section-symbol") << name << addr << size;
    if (typ == SymbolRef::ST_Function)
        s.entry("function") << addr;
}

void macho_symbol(const std::string &name, uint64_t value, ogre_doc &s) {
    s.entry("macho-symbol") << name << value;
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

#include <llvm/Object/SymbolSize.h>

void macho_commands(const macho &obj, ogre_doc &s) {
    for (auto it : obj.load_commands())
        macho_command(obj, it, s);
}

void sections(const macho &obj, ogre_doc &s) {
    for (auto sec : obj.sections())
        section(obj.getSection(sec.getRawDataRefImpl()), s);
}

void symbols(const macho &obj, ogre_doc &s) {
    auto sizes = computeSymbolSizes(obj);
    for (auto sized_sym : sizes) {
        auto sym = sized_sym.first;
        auto er_name = sym.getName();
        auto entry = sym_entry(obj, sym);
        if (!entry || !er_name) continue;
        auto name = (*er_name).str();
        if (is_in_section(entry))
            section_symbol(name, entry->n_value, sized_sym.second, sym.getType(), s);
        else
            macho_symbol(name, entry->n_value, s);
    }
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

void macho_commands(const macho &obj, ogre_doc &s) {
    std::size_t cmd_count = 0;
    if (obj.is64Bit())
        cmd_count = obj.getHeader64().ncmds;
    else
        cmd_count = obj.getHeader().ncmds;
    command_info info = obj.getFirstLoadCommandInfo();
    for (std::size_t i = 0; i < cmd_count; ++i)
        macho_command(obj, info, s);
}

void sections(const macho &obj, ogre_doc &s) {
    auto end = obj.end_sections();
    for (auto it = obj.begin_sections(); it != end; next(it, end))
        section(obj.getSection(it->getRawDataRefImpl()), s);
}

void symbols(const macho &obj, ogre_doc &s) {
    StringRef name;
    uint64_t size;
    SymbolRef::Type typ;
    auto end = obj.end_symbols();
    for (auto it = obj.begin_symbols(); it != end; next(it, end)) {
        auto er_name = it->getName(name);
        auto er_size = it->getSize(size);
        auto er_type = it->getType(typ);
        auto entry   = sym_entry(obj, *it);
        if (er_name || er_size || er_type || !entry) continue;

        if (is_in_section(entry))
            section_symbol(name, entry->n_value, size, typ, s);
         else
            macho_symbol(name, entry->n_value, s);
    }
}

#else
#error LLVM version is not supported
#endif

} // namespace macho_loader

error_or<std::string> load(const llvm::object::MachOObjectFile &obj) {
    using namespace macho_loader;
    ogre_doc s;
    s.raw_entry(macho_declarations);
    s.raw_entry("(file-type macho)");
    s.entry("arch") << arch_of_object(obj);
    macho_commands(obj, s);
    sections(obj, s);
    symbols(obj, s);
    return s.str();
}

} // namespace loader

#endif //  LLVM_MACHO_LOADER_HPP
