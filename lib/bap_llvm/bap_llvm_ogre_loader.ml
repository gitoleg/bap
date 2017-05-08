open Core_kernel.Std
open Bap.Std
open Monads.Std

module Fact = Ogre.Make(Monad.Ident)
module Result = Monad.Result.Error
open Result.Syntax

module Elf_scheme = struct
  open Ogre.Type

  let declare name scheme f = Ogre.declare ~name scheme f

  let off  = "offset" %: int
  let size = "size"   %: int
  let name = "name"   %: str
  let addr = "addr"   %: int

  (** flags that describes an entry behavior *)
  let ld = "load" %: bool
  let r = "read"  %: bool
  let w = "write"   %: bool
  let x = "execute" %: bool

  let symbol_type = "symbol-type" %: str

  (** elf program header as it is in file *)
  let program_header () = declare "program-header"
      (scheme name $ off $ size) Tuple.T3.create

  (** elf program header as it is in memory *)
  let virtual_program_header () = declare "virtual-program-header"
      (scheme name $ addr $ size) Tuple.T3.create

  (** elf program header flags *)
  let program_header_flags () = declare "program-header-flags"
      (scheme name $ ld $ r $ w $ x) (fun name ld r w x -> name,ld,r,w,x)

  (** elf section header *)
  let section_header () = declare "section-header"
      (scheme name $ addr $ size) Tuple.T3.create

  (** elf symbol entry *)
  let symbol_entry () =
    declare "symbol-entry"
      (scheme name $ addr $ size $ symbol_type)
      (fun name addr size typ -> name,addr,size,typ)

end

module Image = struct
  open Image.Scheme
  open Elf_scheme
  open Fact.Syntax

  let segments =
    Fact.foreach Ogre.Query.(begin
        select (from program_header $ virtual_program_header $ program_header_flags)
          ~join:[[field name]]
      end)
      ~f:(fun hdr (_,addr,vsize) (_,ld,r,w,x) ->
          hdr, (addr, vsize), (ld,r,w,x)) >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun ((name,off,size), (addr, vsize), (ld,r,w,x)) ->
          if ld then
              Fact.provide segment addr vsize r w x >>= fun () ->
              Fact.provide mapped addr size off >>= fun () ->
              Fact.provide named_region addr vsize name
          else Fact.return ()) >>= Fact.return

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
    Fact.Seq.iter s ~f:(fun (name, addr, size, sym_type) ->
        if size = Int64.zero then Fact.return ()
        else
          Fact.provide named_symbol addr name >>= fun () ->
          Fact.provide symbol_chunk addr size addr >>= fun () ->
          if sym_type = "STT_FUNC" then
            Fact.provide code_start addr
          else Fact.return ())

  let elf =
    segments >>= fun () ->
    sections >>= fun () ->
    symbols
end

module Loader = struct

  exception Llvm_loader_fail of int

  let _ = Callback.register_exception
      "Llvm_loader_fail" (Llvm_loader_fail 0)

  let to_image_doc doc =
    match Fact.exec Image.elf doc with
    | Ok doc -> Ok (Some doc)
    | Error er -> Error er

  let from_data data =
    try
      let doc = Bap_llvm_binary.bap_llvm_load data in
      Ogre.Doc.from_string doc >>= fun doc -> to_image_doc doc
    with Llvm_loader_fail n -> match n with
      | 1 -> Ok None
      | 2 -> Or_error.error_string "file corrupted"
      | n -> Or_error.errorf "fail with unexpeced error code %d" n

  let from_file path = Bap_fileutils.readfile path |> from_data
end
