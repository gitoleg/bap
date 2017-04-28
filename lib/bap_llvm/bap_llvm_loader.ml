open Core_kernel.Std
open Bap.Std
open Monads.Std

module Fact = Ogre.Make(Monad.Ident)
module Result = Monad.Result.Error

open Bap_llvm_binary

let make_addr arch addr =
  match Arch.addr_size arch with
  | `r32 -> Addr.of_int64 ~width:32 addr
  | `r64 -> Addr.of_int64 ~width:64 addr

let make_segment arch bin ind =
  let name = segment_name bin ind in
  let perm = Backend.([
      segment_is_readable bin ind, R;
      segment_is_writable bin ind, W;
      segment_is_executable bin ind, X ] |>
      List.filter_map ~f:(fun (e, p) -> if e then Some p else None) |>
      List.reduce ~f:(fun p1 p2 -> Or (p1, p2))) in
  let off = segment_offset bin ind |> Int64.to_int_exn in
  let size = segment_size bin ind in
  let location = Location.Fields.create
      ~addr:(segment_addr bin ind |> make_addr arch)
      ~len:(Int64.to_int_exn size) in
  match perm with
  | Some perm when size <> 0L ->
    Backend.Segment.Fields.create ~name ~perm ~off ~location |>
    Option.some
  | _ -> None

let make_symbol arch bin ind =
  let name = symbol_name bin ind in
  let is_function = symbol_is_fun bin ind in
  let is_debug = symbol_is_debug bin ind in
  let addr = symbol_addr bin ind in
  let size = symbol_size bin ind in
  let locations = Location.Fields.create
      ~addr:(make_addr arch addr)
      ~len:(Int64.to_int_exn size), [] in
  if size <> 0L then
    Backend.Symbol.Fields.create ~name ~is_function ~is_debug ~locations |>
    Option.some
  else None

let make_section arch bin ind =
  let name = section_name bin ind in
  let addr = section_addr bin ind in
  let size = section_size bin ind in
  let location = Location.Fields.create
      ~addr:(make_addr arch addr)
      ~len:(Int64.to_int_exn size) in
  Some (Backend.Section.Fields.create ~name ~location)

let collection arch bin (f,max) =
  List.init (max bin) ~f:(f arch bin) |>
  List.filter_opt

let nonempty = function
  | [] -> None
  | hd :: tl -> Some (hd, tl)

let segment = make_segment, segments_number
let section = make_section, sections_number
let symbol  = make_symbol, symbols_number

let with_err_message msg f x = match f x with
  | None -> eprintf "%s\n" msg; None
  | r -> r

let arch_of_string arch =
  with_err_message
    (sprintf "unknown arch %s\n" arch) Arch.of_string arch

let collect_segments arch bin =
  collection arch bin segment |>
  with_err_message "segments list is empty" nonempty

let create_binary data =
  with_err_message "create binary failed" create_binary data

let of_data data : Backend.Img.t option =
  let open Option in
  create_binary data >>= fun bin ->
  arch_of_string (image_arch bin) >>= fun arch ->
  collect_segments arch bin >>= fun segments ->
  let entry = make_addr arch (image_entry bin) in
  let symbols = collection arch bin symbol in
  let sections = collection arch bin section in
  Some (Backend.Img.Fields.create
          ~arch ~entry ~segments ~symbols ~sections)

module Check = struct
  open Fact.Syntax
  open Ogre.Type

  let loader_feedback what =
    let msg = "message"  %: str in
    Ogre.declare ("loader-" ^ what) (scheme msg) ident

  let loader_error   () = loader_feedback "error"
  let loader_warning () = loader_feedback "warning"

  let error = Fact.request loader_error

  let make =
    Fact.foreach Ogre.Query.(select (from loader_warning)) ~f:ident >>=
    fun ws -> Seq.iter ~f:(Format.printf "loader-warning: %s\n") ws;
    Fact.request loader_error
end

module Ogre_loader = struct
  open Result.Syntax

  exception Llvm_loader_fail of int

  let _ = Callback.register_exception
      "Llvm_loader_fail" (Llvm_loader_fail 0)

  let from_data data =
    try
      let s = bap_llvm_load data in
      match Ogre.Doc.from_string s with
      | Error er -> Or_error.errorf "can't construct doc"
      | Ok doc ->
        Fact.eval Check.make doc >>= function
        | None -> Ok (Some doc)
        | Some er -> Or_error.error_string er
    with (Llvm_loader_fail n) ->
    match n with
    | 1 -> Ok None  (** loader doesn't suppot a give file type *)
    | 2 -> Or_error.error_string "file corrupted"
    | n -> Or_error.errorf "fail with unexpeced error code %d" n

  let from_file path = Bap_fileutils.readfile path |> from_data

end

let init () =
  Image.register_loader ~name:"llvm" (module Ogre_loader);
  match Image.register_backend ~name:"llvm-legacy" of_data with
  | `Ok -> Ok ()
  | `Duplicate -> Or_error.errorf "llvm-legacy loader: duplicate name"
