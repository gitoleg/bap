open Core_kernel.Std
open Monads.Std

module Fact = Ogre.Make(Monad.Ident)
module Result = Monad.Result.Error
open Result.Syntax

module Elf_scheme = struct
  open Ogre.Type

  let declare name scheme f = Ogre.declare ~name scheme f

  (** v - for virtual  *)
  let off = "offset"  %: int
  let size = "size"  %: int
  let v_size = "v-size" %: int
  let v_addr = "v-addr" %: int
  let name  = "name"   %: str
  let ld = "load" %: bool
  let r = "read"  %: bool
  let w = "write"  %: bool
  let x = "execute" %: bool
  let is_fun = "is-funciton" %: bool

  let program_header () =
    declare "program-header" (scheme off $ size)
      (fun offset size -> offset, size)

  let virtual_pheader () =
    declare "virtual-pheader" (scheme off $ size $ v_addr $ v_size)
      (fun offset size addr vsize -> offset, size, addr, vsize)

  let pheader_flags () =
    declare "pheader-flags"
      (scheme off $ size $ ld $ r $ w $ x)
      (fun offset size ld r w x -> offset, size, ld, r, w, x)

  let section_header () =
    declare "section-header"
      (scheme name $ v_addr $ size)
      (fun name addr size -> name,addr,size)

  let symbol_entry () =
    declare "symbol-entry"
      (scheme name $ v_addr $ size $ is_fun)
      (fun name addr size is_fun -> name,addr,size,is_fun)
end

module Image_ogre_of_elf = struct
  open Bap_image.Scheme
  open Elf_scheme
  open Fact.Syntax

  let segments =
    let name_of_number n = sprintf "%02d" n in
    Fact.foreach Ogre.Query.(begin
        select (from program_header $ virtual_pheader $ pheader_flags)
          ~join:[[field off];[field size]]
      end)
      ~f:(fun (off, size) (_,_,addr,vsize) (_,_,ld,r,w,x) ->
          (off,size), (addr, vsize), (ld,r,w,x)) >>= fun s ->
    Fact.Seq.fold s ~init:0
      ~f:(fun n ((off,size), (addr, vsize), (ld,r,w,x)) ->
          if ld then
            Fact.provide segment addr vsize r w x >>= fun () ->
            Fact.provide mapped addr size off >>= fun () ->
            Fact.provide named_region addr vsize (name_of_number n) >>= fun () ->
            Fact.return (n + 1)
          else Fact.return (n + 1)) >>= fun _ -> Fact.return ()

  let sections =
    Fact.foreach Ogre.Query.(select (from section_header))
      ~f:ident >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun (name, addr, size) ->
          Fact.provide section addr size >>= fun () ->
          Fact.provide named_region addr size name)

  let symbols =
    Fact.foreach Ogre.Query.(select (from symbol_entry))
      ~f:ident >>= fun s ->
    Fact.Seq.iter s ~f:(fun (name, addr, size, is_fun) ->
        Fact.provide named_symbol addr name >>= fun () ->
        Fact.provide symbol_chunk addr size addr >>= fun () ->
        if is_fun then
          Fact.provide code_start addr
        else Fact.return ())

  let img =
    segments >>= fun () ->
    sections >>= fun () ->
    symbols
end

let to_image_doc doc =
  printf "to image!\n";
  match Fact.exec Image_ogre_of_elf.img doc with
  | Ok doc ->
    printf "has doc!\n";
    (* printf "%s\n" @@ Ogre.Doc.to_string doc; *)
    Ok (Some doc)
  | Error er -> Error er

module Loader = struct
  let _from_data data =
    Bap_llvm_binary.bap_llvm_load data |>
    Ogre.Doc.from_string >>= fun doc -> to_image_doc doc

  let from_data data =
    let str = Bap_llvm_binary.bap_llvm_load data  in
    (* printf "%s\n" str; *)
    Ogre.Doc.from_string str >>= fun doc -> to_image_doc doc

  let from_file path = Bap_fileutils.readfile path |> from_data
end
