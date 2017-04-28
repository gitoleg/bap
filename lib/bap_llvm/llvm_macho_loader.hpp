#ifndef LLVM_MACHO_LOADER_HPP
#define LLVM_MACHO_LOADER_HPP

#include <sstream>

#include <llvm/Object/MachO.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Object/SymbolSize.h>

#include "llvm_loader_utils.hpp"
#include "llvm_error_or.hpp"


namespace loader {


using namespace llvm;
using namespace llvm::object;

namespace macho {

bool is_main(const MachOObjectFile::LoadCommandInfo &info) {
    return (info.C.cmd == MachO::LoadCommandType::LC_MAIN);
}

template <typename Cmds>
error_or<uint64_t> image_entry(const Cmds &commands) {
    auto it = std::find_if(commands.begin(), commands.end(), is_main);
    if (it == commands.end())
        return failure("LC_MAIN not found, binary version < 10.8");
    const MachO::entry_point_command *entry_cmd =
        reinterpret_cast<const MachO::entry_point_command*>(it->Ptr);
    return success(entry_cmd->entryoff);
}


#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

iterator_range<MachOObjectFile::load_command_iterator> load_commands(const MachOObjectFile &obj) {
    return obj.load_commands();
}

void provide_sections(utils::ostream &s, const MachOObjectFile &obj) {
    for (auto sec : obj.sections()) {
        StringRef name;
        if (auto er = sec.getName(name)) { s.fail(er.message()); return; };
        utils::provide_section(s, name.str(), sec.getAddress(), sec.getSize());
    }
}

void provide_symbols(utils::ostream &s, const MachOObjectFile &obj) {
    auto syms = computeSymbolSizes(obj);
    for (std::pair<SymbolRef, uint64_t> sized_sym : syms) {
        auto sym = sized_sym.first;
        auto size = sized_sym.second;
        auto name = sym.getName();
        auto addr = sym.getAddress();
        if (!name) { utils::skip_symbol(s, name.getError()); continue; }
        if (!addr) { utils::skip_symbol(s, addr.getError()); continue; }
        bool is_fun = (sym.getType() == SymbolRef::ST_Function);
        utils::provide_symbol(s, name.get().str(), addr.get(), size, is_fun);
    }
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

typedef MachOObjectFile::LoadCommandInfo command_info;
typedef std::vector<command_info> macho_commands;

macho_commands load_commands(const MachOObjectFile &obj) {
    std::size_t cmd_count = 0;
    if (obj.is64Bit())
        cmd_count = obj.getHeader64().ncmds;
    else
        cmd_count = obj.getHeader().ncmds;
    macho_commands cmds(cmd_count);
    command_info info = obj.getFirstLoadCommandInfo();
    for (std::size_t i = 0; i < cmd_count; ++i) {
        cmds.push_back(info);
        info = obj.getNextLoadCommandInfo(info);
    }
    return cmds;
}

void provide_sections(utils::ostream &s, const MachOObjectFile &obj) {
    std::vector<SectionRef> secs;
    error_code ec;
    StringRef name;
    uint64_t addr, size;
    for (auto it = obj.begin_sections(); it != obj.end_sections(); it.increment(ec)) {
        if (ec) { return s.fail(ec.message()); return; }
        if (auto er = it->getName(name))    { utils::skip_symbol(s, er); return; }
        if (auto er = it->getAddress(addr)) { utils::skip_symbol(s, er); return; }
        if (auto er = it->getSize(size))    { utils::skip_symbol(s, er); return; }
        utils::provide_section(s, name.str(), addr, size);
    }
}

void provide_symbols(utils::ostream &s, const MachOObjectFile& obj) {
    error_code ec;
    for (auto it = obj.begin_symbols(); it != obj.end_symbols(); it.increment(ec)) {
        if (ec) { return s.fail(ec.message()); return; }
        StringRef name;
        uint64_t addr, size;
        SymbolRef::Type kind;
        if (auto er = it->getName(name))    { utils::skip_symbol(s, er); return; }
        if (auto er = it->getAddress(addr)) { utils::skip_symbol(s, er); return; }
        if (auto er = it->getSize(size))    { urils::skip_symbol(s, er); return; }
        if (auto er = it->getType(kind))    { utils::skip_symbol(s, er); return; }
        utils::provide_symbol(s, name.str(), addr, size, (kind == SymbolRef::ST_Function));
    }
}


#else
#error LLVM version is not supported
#endif

error_or<uint64_t> image_entry(const MachOObjectFile& obj) {
    return image_entry(load_commands(obj));
}

template <typename Cmd>
void provide_segment(utils::ostream &s, const Cmd &cmd) {
    int off = static_cast<int>(cmd.fileoff);
    bool r = static_cast<bool>(cmd.initprot & MachO::VM_PROT_READ);
    bool w = static_cast<bool>(cmd.initprot & MachO::VM_PROT_WRITE);
    bool x = static_cast<bool>(cmd.initprot & MachO::VM_PROT_EXECUTE);
    *s << scheme::segment(cmd.vmaddr, cmd.vmsize, r, w, x);
    *s << scheme::mapped(cmd.vmaddr, cmd.filesize, off);
    *s << scheme::named_region(cmd.vmaddr, cmd.vmsize, cmd.segname);
}

void provide_segments(utils::ostream &s, const MachOObjectFile &obj) {
    for (auto it : load_commands(obj)) {
        if (it.C.cmd == MachO::LoadCommandType::LC_SEGMENT_64)
            provide_segment(s, obj.getSegment64LoadCommand(it));
        if (it.C.cmd == MachO::LoadCommandType::LC_SEGMENT)
            provide_segment(s, obj.getSegmentLoadCommand(it));
    }
}

} // namespace macho

error_or<std::string> load(const MachOObjectFile &obj) {
    utils::ostream s = success(std::ostringstream());
    auto arch_str = Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch()));
    auto entry = macho::image_entry(obj);
    if (!entry) return failure(entry.message());
    *s << scheme::declare();
    *s << scheme::arch(arch_str);
    *s << scheme::entry_point(*entry);
    macho::provide_segments(s, obj);
    macho::provide_sections(s, obj);
    if (!s) return failure(s.message());
    macho::provide_symbols(s, obj);
    if (!s) return failure(s.message());
    return std::move(success(s->str()) << s.warnings());
}

} // namespace loader

#endif // LLVM_MACHO_LOADER_HPP
