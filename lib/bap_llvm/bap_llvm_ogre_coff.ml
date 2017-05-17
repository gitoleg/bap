open Core_kernel.Std
open Bap.Std

open Bap_llvm_ogre_types

module Scheme = struct
  open Ogre.Type

  (** coff section in file *)
  let section_header () =
    Ogre.declare ~name:"section-header" (scheme name $ off $ size)
      Tuple.T3.create

  (** coff section in memory *)
  let virtual_section_header () =
    Ogre.declare ~name:"virtual-section-header"
      (scheme name $ addr $ size) Tuple.T3.create

  (** coff section access flags *)
  let section_flags () =
    Ogre.declare ~name:"section-flags" (scheme name $ r $ w $ x)
      (fun name r w x -> name, (r,w,x))

  (** coff section that contains code *)
  let code_content () = Ogre.declare ~name:"code-content" (scheme name) ident

  (** coff symbol  *)
  let symbol () =
    Ogre.declare ~name:"symbol" (scheme name $ addr $ size) Tuple.T3.create

  (** coff symbol that is a function *)
  let function_ () =
    Ogre.declare ~name:"function" (scheme name $ addr) Tuple.T2.create
end

module Make(Fact : Ogre.S) = struct
  open Image.Scheme
  open Scheme
  open Fact.Syntax

  let segments =
    Fact.foreach Ogre.Query.(
        select (from section_header
                $ virtual_section_header
                $ section_flags
                $ code_content)
          ~join:[[field name]])
      ~f:(fun (name, start, size) (_,addr,vsize) (_,rwx) _  ->
          name,start,size,addr,vsize,rwx) >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun (name, start, size, addr, vsize, (r,w,x)) ->
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
    Fact.foreach Ogre.Query.(select (from symbol))
      ~f:ident >>= fun s ->
    Fact.Seq.iter s ~f:(fun (name, addr, size) ->
        if size = 0L then Fact.return ()
        else
          Fact.provide named_symbol addr name >>= fun () ->
          Fact.provide symbol_chunk addr size addr >>= fun () ->
          Fact.request function_
            ~that:(fun (n,a) -> a = addr && n = name) >>= fun f ->
          if f <> None then Fact.provide code_start addr
          else Fact.return ())

  let image =
    segments >>= fun () ->
    sections >>= fun () ->
    symbols

end
