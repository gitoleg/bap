open Core_kernel.Std
open Bap.Std
open Monads.Std
open Or_error
open Bap_llvm_ogre_types
open Bap_llvm_ogre_types.Scheme

module Dispatch(M : Monad.S) = struct
  module Fact = struct
    include Ogre.Make(M)
    type 'a m = 'a M.t
  end

  open Fact.Syntax

  module Elf = Bap_llvm_ogre_elf.Make(Fact)
  module Elf_rel = Bap_llvm_ogre_elf.Relocatable.Make(Fact)
  module Coff = Bap_llvm_ogre_coff.Make(Fact)
  module Macho = Bap_llvm_ogre_macho.Make(Fact)

  module type A = Bap_llvm_ogre_types.S with type 'a m := 'a Fact.t

  type typ = Elf | Coff | Macho | Unknown [@@deriving sexp]

  let filetype_of_string s =
    try
      typ_of_sexp (Sexp.of_string s)
    with _ -> Unknown

  let provide x =
    let module A = (val x : A) in
    A.segments >>= fun () ->
    A.sections >>= fun () ->
    A.symbols

  let image =
    Fact.require file_type >>= fun s ->
    match filetype_of_string s with
    | Elf ->
      Fact.require Bap_llvm_elf_scheme.is_relocatable >>= fun is_rel ->
      if is_rel then
        provide (module Elf_rel)
      else provide (module Elf)
    | Coff -> provide (module Coff)
    | Macho -> provide (module Macho)
    | Unknown -> Fact.failf "file type is not supported" ()

end

module Loader = struct

  include Dispatch(Monad.Ident)

  exception Llvm_loader_fail of int

  let _ = Callback.register_exception
      "Llvm_loader_fail" (Llvm_loader_fail 0)

  let to_image_doc doc =
    match Fact.exec image doc with
    | Ok doc -> Ok (Some doc)
    | Error er -> Error er

  let from_data data =
    try
      let doc = Bap_llvm_binary.bap_llvm_load data in
      Ogre.Doc.from_string doc >>= fun doc ->
      to_image_doc doc
    with Llvm_loader_fail n -> match n with
      | 1 -> Ok None
      | 2 -> Or_error.error_string "file corrupted"
      | n -> Or_error.errorf "fail with unexpected error code %d" n

  let map_file path =
    let fd = Unix.(openfile path [O_RDONLY] 0o400) in
    try
      let size = Unix.((fstat fd).st_size) in
      let data = Bigstring.map_file ~shared:false fd size in
      Unix.close fd;
      Ok data
    with exn ->
      Unix.close fd;
      Or_error.errorf "unable to process file %s" path

  let from_file path =
    Or_error.(map_file path >>= from_data)

end

let init () =
  Image.register_loader ~name:"llvm" (module Loader)
