open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_sema.Std
open Monads.Std

include Bap_self.Create()

module Fact = Ogre.Make(Monad.Ident)

module Ext = struct
  open Image.Scheme
  open Fact.Syntax

  let symbols  =
    Fact.collect Ogre.Query.(
        select (from external_symbol)) >>= fun s ->
    Fact.return (Seq.to_list s)
end

let external_symbols doc =
  match Fact.eval Ext.symbols doc with
  | Ok ext -> ext
  | Error er ->
    error "%a" Error.pp er;
    []


class sub_symbolizer names = object
  inherit Term.mapper

  method! map_sub s =
    if Term.(has_attr s synthetic) then
      let name =
        List.find ~f:(fun (tid, name) -> Term.tid s = tid) names in
      match name with
      | None -> s
      | Some (tid,name) ->
        Tid.set_name tid name;
        Ir_sub.with_name s name
    else s
end

class jmp_finder = object
  inherit [jmp term list] Term.visitor
  method! enter_jmp jmp jmps = jmp :: jmps
end

let direct_call_of_jmp jmp = match Ir_jmp.kind jmp with
  | Call call ->
    begin
      match Call.target call with
      | Direct tid -> Some (call, tid)
      | _ -> None
    end
  | _ -> None

let target_tid jmp = match direct_call_of_jmp jmp with
  | None -> None
  | Some (_,tid) -> Some tid

(** [external_calls insns exts] - returns a list of (addr, name),
    where [addr] is an address of call instruction and [name] is
    an external symbol name that is referenced by this call.
    A name is found from list of offsets [exts] that are associated
    with some name.
    e8 00 00 00 00
    |  |        |
    |  |        max address of call instruction
    |  |
    |  relocation offset, that assoicated with some name
    |
    min address of call instruction.
*)
let external_calls insns exts =
  Seq.filter_map ~f:(fun (mem, insn) ->
      if Insn.(is call insn) then
        let min,max = Memory.(min_addr mem, max_addr mem) in
        List.find_map exts ~f:(fun (off, name) ->
            let off =
              Addr.of_int64 ~width:(Addr.bitwidth min) off in
            if min <= off && off <= max then Some (min, name)
            else None)
      else None) insns |>
  Seq.to_list

let find_destinations calls jmp =
  let open Option in
  Term.get_attr jmp address >>= fun addr ->
  List.find ~f:(fun (a,n) -> a = addr) calls >>= fun (_,name) ->
  target_tid jmp >>= fun tid ->
  Some (tid,name)

(** [find_externals_names program insn externals] - returns a list of
    (tid, name) where every [tid] denotes a sythetic subroutine term
    that must be renamed to [name]. *)
let find_externals_names prg insns exts =
  let calls = external_calls insns exts in
  let jumps = List.rev @@ (new jmp_finder)#run prg [] in
  List.filter_map ~f:(find_destinations calls) jumps

let name prg exts = (new sub_symbolizer exts)#run prg

let is_synthetic_sub prg tid =
  match Term.find sub_t prg tid with
  | Some sub -> Term.(has_attr sub synthetic)
  | None -> false

class relinker src_prg names = object
  inherit Term.mapper

  method! map_jmp jmp = match direct_call_of_jmp jmp with
    | None -> jmp
    | Some (call, tid) ->
      if is_synthetic_sub src_prg tid then
        match String.Map.find names (Tid.name tid) with
        | Some unq_tid when not (Tid.equal unq_tid tid) ->
          let call = Call.with_target call (Direct unq_tid) in
          Ir_jmp.with_kind jmp (Call call)
        | _ -> jmp
      else jmp
end

(** [reduce prg] removes duplicated synthetic subroutines,
    that appeared during program lifting are external calls
    resolving. *)
let reduce prg =
  let jumps = List.rev @@ (new jmp_finder)#run prg [] in
  let targets = List.filter_map ~f:target_tid jumps in
  let unique, duplicates =
    List.fold targets ~init:(String.Map.empty, [])
      ~f:(fun (unq, rem) tid ->
          let name = Tid.name tid in
          if String.Map.mem unq name then unq, tid :: rem
          else String.Map.add unq name tid, rem) in
  let prg = List.fold duplicates ~init:prg
      ~f:(fun prg tid ->
          if is_synthetic_sub prg tid then
            Term.remove sub_t prg tid
          else prg) in
  (new relinker prg unique)#run prg

let resolve doc insns prg =
  external_symbols doc |>
  find_externals_names prg insns |>
  name prg |>
  reduce
