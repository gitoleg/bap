open Core_kernel.Std
open Bap.Std
open Monads.Std
open Or_error
open Bap_llvm_ogre_types

module Make(M : Monad.S) = struct
  include Ogre.Make(M)
  type 'a m = 'a M.t
end

module Dispatch(M : Monad.S) = struct
  module Fact = Make(Monad.Ident)
  module Elf = Bap_llvm_ogre_elf.Make(Fact)
  module Coff = Bap_llvm_ogre_coff.Make(Fact)
  open Fact.Syntax

  let image =
    Elf.probe >>= fun x ->
    if x then Elf.image
    else
      Coff.probe >>= fun x ->
      if x then Coff.image
      else
      Fact.failf "file type is not supported" ()
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
      Ogre.Doc.from_string doc >>= fun doc -> to_image_doc doc
    with Llvm_loader_fail n -> match n with
      | 1 -> Ok None
      | 2 -> Or_error.error_string "file corrupted"
      | n -> Or_error.errorf "fail with unexpeced error code %d" n

  let from_file path =
    let fd = Unix.(openfile path [O_RDONLY] 0o400) in
    try
      let size = Unix.((fstat fd).st_size) in
      let data = Bigstring.map_file ~shared:false fd size in
      Unix.close fd;
      from_data data
    with exn ->
      Unix.close fd;
      Or_error.errorf "unable to process file %s" path
end
