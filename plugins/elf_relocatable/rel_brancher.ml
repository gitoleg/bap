open Core_kernel.Std
open Bap_future.Std
open Bap.Std
open Image
open Monads.Std

include Self()

module Fact = Ogre.Make(Monad.Ident)

module Rel = struct
  open Image.Scheme
  open Bap_llvm_elf_scheme
  open Fact.Syntax

  let relocations =
    Fact.collect Ogre.Query.(select (from symbol_reference)) >>= fun s ->
    Fact.return (Seq.to_list s)

  let external_symbols  =
    Fact.collect Ogre.Query.(
        select (from external_symbol)) >>= fun s ->
    Fact.return (Seq.to_list s)

  let relocations =
    relocations >>= fun rels ->
    external_symbols >>= fun ext ->
    Fact.return (rels, ext)

end

let relocations doc =
  match Fact.eval Rel.relocations doc with
  | Ok rel -> rel
  | Error er ->
    error "%a" Error.pp er;
    [],[]

let width_of_mem m = Word.bitwidth (Memory.min_addr m)

let get mem data =
  let width = width_of_mem mem in
  List.find_map data ~f:(fun (off,value) ->
      let off = Addr.of_int64 ~width off in
      Option.some_if (Memory.contains mem off) value)

let relocate rels mem =
  let width = width_of_mem mem in
  Option.map (get mem rels) ~f:(Addr.of_int64 ~width)

let nullify_call exts mem =
  let width = width_of_mem mem in
  Option.map (get mem exts) ~f:(fun _ -> Addr.zero width)

let contains_unresolved (rels,exts) mem =
  Option.is_some (get mem rels) || Option.is_some (get mem exts)

let resolve (rels, exts) mem default =
  match relocate rels mem with
  | Some a -> a
  | None ->
    Option.value_map ~default ~f:ident (nullify_call exts mem)

let resolve_jumps mem rels dests =
  List.map ~f:(function
      | Some addr, `Jump -> Some (resolve rels mem addr), `Jump
      | x -> x) dests

let resolve_dests b rels mem insn =
  let dests = Brancher.resolve b mem insn in
  if not (contains_unresolved rels mem) then dests
  else
    let has_fall = List.exists ~f:(fun (_,x) -> x = `Fall) dests in
    let dests = resolve_jumps mem rels dests in
    if has_fall then dests
    else
      let fall = Some (Addr.succ @@ Memory.max_addr mem), `Fall in
      fall :: resolve_jumps mem rels dests

let create arch spec =
  let rels = relocations spec in
  let b = Brancher.of_bil arch in
  Ok (Brancher.create (resolve_dests b rels))

let () =
  let open Project.Info in
  Stream.Variadic.(apply (args arch $ spec) ~f:create) |>
  Brancher.Factory.register "elf-relocatable"
