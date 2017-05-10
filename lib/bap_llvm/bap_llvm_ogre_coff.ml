open Core_kernel.Std
open Bap.Std

open Bap_llvm_ogre_types

module Scheme = struct
  open Ogre.Type

  include Common_fields

  let declare name scheme f = Ogre.declare ~name scheme f

  let coff () = declare "coff-format" (scheme flag) ident

  let section_header () =
    declare "section-header" (scheme name $ off $ size)
      Tuple.T3.create

  let virtual_section_header () =
    declare "virtual-section-header" (scheme name $ addr $ size) Tuple.T3.create

  let section_access () =
    declare "section-access" (scheme name $ r $ w $ x)
      (fun name r w x -> name, (r,w,x))

  let code_content () = declare "code-content" (scheme name) ident

  let function_ () =
    declare "function" (scheme name $ addr $ size) Tuple.T3.create

end

module Make(Fact : Ogre.S) = struct
  open Image.Scheme
  open Scheme
  open Fact.Syntax

  let segments =
    Fact.foreach Ogre.Query.(
        select (from section_header
                $ virtual_section_header
                $ section_access
                $ code_content)
          ~join:[[field name]])
      ~f:(fun (name, start, size) (_,addr,vsize) (_,rwx) _  ->
          name,start,size,addr,vsize,rwx) >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun (name, start, size, addr, vsize, (r,w,x)) ->
          printf "we are here\n";
          Fact.provide segment addr vsize r w x >>= fun () ->
          Fact.provide mapped addr size start)

  let sections =
    Fact.foreach Ogre.Query.(
        select (from section_header $ virtual_section_header)
          ~join:[[field name]])
      ~f:(fun (name,_,_) (_,addr,size) -> name,addr,size) >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun (name, addr, size) ->
          Fact.provide section addr size >>= fun () ->
          Fact.provide named_region addr size name)

  let symbols =
    Fact.foreach Ogre.Query.(select (from function_))
      ~f:ident >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun (name, addr, size) ->
          Fact.provide named_symbol addr name >>= fun () ->
          Fact.provide code_start addr >>= fun () ->
          Fact.provide symbol_chunk addr size addr)

  let image =
    segments >>= fun () ->
    sections >>= fun () ->
    symbols

  let probe = Fact.request coff >>= fun x ->
    Fact.return (x <> None)

end
