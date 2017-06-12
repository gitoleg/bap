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

let relocate rels mem =
  let min = Memory.min_addr mem in
  let max = Memory.max_addr mem in
  let width = Word.bitwidth min in
  List.find_map rels ~f:(fun (off,value) ->
      let off = Addr.of_int64 ~width off in
      if (min <= off && off <= max) then
        Some (Addr.of_int64 ~width value)
      else None)

let nullify_call exts mem =
  let min = Memory.min_addr mem in
  let max = Memory.max_addr mem in
  let width = Word.bitwidth min in
  List.find_map exts ~f:(fun (off, _) ->
      let off = Addr.of_int64 ~width off in
      if (min <= off && off <= max) then
        Some (Addr.zero width)
      else None)

let relocate (rels, exts) mem jmp_addr =
  match relocate rels mem with
  | Some addr -> addr
  | None ->
    Option.value_map (nullify_call exts mem) ~f:ident ~default:jmp_addr

let relocate_dests b rels mem insn =
  List.map ~f:(function
      | Some addr, `Jump -> Some (relocate rels mem addr), `Jump
      | x -> x) (Brancher.resolve b mem insn)

let create arch spec =
  let rels = relocations spec in
  let b = Brancher.of_bil arch in
  Ok (Brancher.create (relocate_dests b rels))

let () =
  let open Project.Info in
  Stream.Variadic.(apply (args arch $ spec) ~f:create) |>
  Brancher.Factory.register "elf-relocatable"
