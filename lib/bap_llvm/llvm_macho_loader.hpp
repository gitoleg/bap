#ifndef LLVM_MACHO_LOADER_HPP
#define LLVM_MACHO_LOADER_HPP

#include <sstream>

#include <llvm/Object/MachO.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Object/SymbolSize.h>

#include "llvm_loader_scheme.hpp"
#include "llvm_error_or.hpp"

namespace macho_loader {

using namespace llvm;
using namespace llvm::object;

typedef error_or<std::ostringstream> ostream;
//typedef llvm::object::MachOObjectFile macho;

template <typename Secs>
void provide_sections(ostream &s, const Secs &sections) {
    for (auto sec : sections) {
        StringRef name;
        if (auto er = sec.getName(name)) { s.fail(er.message()); return; };
        *s << scheme::section(sec.getAddress(), sec.getSize());
        *s << scheme::named_region(sec.getAddress(), sec.getSize(), name.str());
    }
}

void provide_symbol(ostream &s, const std::string &name, uint64_t addr, uint64_t size, bool is_fun) {
    *s << scheme::named_symbol(addr, name);
    if (is_fun) {
        *s << scheme::code_start(addr);
        *s << scheme::symbol_chunk(addr, size, addr);
    }
}

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

template <typename Er>
void skip_symbol(ostream &s, const Er &er) {
    s.warning() << "skipping symbols: " << er.message();
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

iterator_range<MachOObjectFile::load_command_iterator> load_commands(const MachOObjectFile &obj) {
    return obj.load_commands();
}

void provide_sections(ostream &s, const MachOObjectFile &obj) {
    provide_sections(s, obj.sections());
}

void provide_symbols(ostream &s, const MachOObjectFile &obj) {
    auto syms = computeSymbolSizes(obj);
    for (std::pair<SymbolRef, uint64_t> sized_sym : syms) {
        auto sym = sized_sym.first;
        auto size = sized_sym.second;
        auto name = sym.getName();
        auto addr = sym.getAddress();
        if (!name) { skip_symbol(s, name.getError()); continue; }
        if (!addr) { skip_symbol(s, addr.getError()); continue; }
        bool is_fun = (sym.getType() == SymbolRef::ST_Function);
        provide_symbol(s, name.get().str(), addr.get(), size, is_fun);
    }
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

typedef macho::LoadCommandInfo command_info;
typedef std::vector<command_info> macho_commands;

macho_commands load_commands(const macho& obj) {
    macho_commands cmds;
    command_info info = obj.getFirstLoadCommandInfo();
    for (std::size_t i = 0; i < cmd_count; ++i) {
        cmds.push_back(info);
        info = obj.getNextLoadCommandInfo(info);
    }
    return cmds;
}

void provide_sections(ostream &s, const MachOObjectFile &obj) {
    std::vector<SectionRef> secs;
    error_code ec;
    for (auto it = obj.begin_sections; it != obj.end_sectons(); it.increment(ec)) {
        if (ec) { return s.fail(ec.message()); return }
        secs.push_back(*it);
    }
    provide_sections(s, secs);
}

error_or<symbols_sizes> getSymbolSizes(const MachOObjectFile& obj) {
    symbols_sizes sizes;
    error_code ec;
    fill_symbols(obj.begin_symbols(), obj.end_symbols(), sizes, ec);
    return success(std::move(sizes));
}

void provide_symbols(ostream &s, const MachOObjectFile& obj) {
    error_code ec;
    for (auto it = obj.begin_symbols(); it != obj.end_symbols(); it.increment(ec)) {
        if (ec) { return s.fail(ec.message()); return }
        StringRef name;
        uint64_t addr, size;
        SymbolRef::Type kind;
        if (auto er = sym.getName(name))    { skip_symbol(s, er); return; }
        if (auto er = sym.getAddress(addr)) { skip_symbol(s, er); return; }
        if (auto er = sym.getSize(size))    { skip_symbol(s, er); return; }
        if (auto er = sym.getType(kind))    { skip_symbol(s, er); return; }
        provide_symbol(s, name.str(), addr, size, (kind == SymbolRef::ST_Function));
    }
}


#else
#error LLVM version is not supported
#endif

error_or<uint64_t> image_entry(const MachOObjectFile& obj) {
    return image_entry(load_commands(obj));
}

//TODO: check what size used and where
template <typename Cmd>
void provide_segment(ostream &s, const Cmd &cmd) {
    int off = static_cast<int>(cmd.fileoff);
    bool r = static_cast<bool>(cmd.initprot & MachO::VM_PROT_READ);
    bool w = static_cast<bool>(cmd.initprot & MachO::VM_PROT_WRITE);
    bool x = static_cast<bool>(cmd.initprot & MachO::VM_PROT_EXECUTE);
    *s << scheme::segment(cmd.vmaddr, cmd.vmsize, r, w, x);
    *s << scheme::mapped(cmd.vmaddr, cmd.filesize, off);
    *s << scheme::named_region(cmd.vmaddr, cmd.vmsize, cmd.segname);
}

void provide_segments(ostream &s, const MachOObjectFile &obj) {
    for (auto it : load_commands(obj)) {
        if (it.C.cmd == MachO::LoadCommandType::LC_SEGMENT_64)
            provide_segment(s, obj.getSegment64LoadCommand(it));
        if (it.C.cmd == MachO::LoadCommandType::LC_SEGMENT)
            provide_segment(s, obj.getSegmentLoadCommand(it));
    }
}

error_or<std::string> load(const MachOObjectFile &obj) {
    ostream s = success(std::ostringstream());
    auto arch_str = Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch()));
    auto entry = image_entry(obj);
    if (!entry) return failure(entry.message());
    *s << scheme::declare();
    *s << scheme::arch(arch_str);
    *s << scheme::entry_point(*entry);
    provide_segments(s, obj);
    provide_sections(s, obj);
    if (!s) return failure(s.message());
    provide_symbols(s, obj);
    if (!s) return failure(s.message());
    return std::move(success(s->str()) << s.warnings());
}


} // namespace macho_loader

#endif // LLVM_MACHO_LOADER_HPP
