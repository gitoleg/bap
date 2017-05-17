open Core_kernel.Std
open Bap.Std
open Monads.Std

open Bap_llvm_ogre_types

module Scheme = struct
  open Ogre.Type

  (** flags that describes an entry behavior *)
  let ld = "load" %: bool

  let declare name scheme f = Ogre.declare ~name scheme f

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
    declare "symbol-entry" (scheme name $ addr $ size) Tuple.T3.create

  (** elf symbols that are functions *)
  let code_entry () = declare "code-entry" (scheme name $ addr) Tuple.T2.create

end

module Make(Fact : Ogre.S) = struct
  open Image.Scheme
  open Scheme
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
    Fact.Seq.iter s ~f:(fun (name, addr, size) ->
        if size = 0L then Fact.return ()
        else
          Fact.provide named_symbol addr name >>= fun () ->
          Fact.provide symbol_chunk addr size addr >>= fun () ->
          Fact.request code_entry
            ~that:(fun (n,a) -> a = addr && n = name) >>= fun f ->
          if f <> None then Fact.provide code_start addr
          else Fact.return ())

  let image =
    segments >>= fun () ->
    sections >>= fun () ->
    symbols

end
