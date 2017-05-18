open Core_kernel.Std
open Bap.Std
open Bap_llvm_ogre_types

module Scheme = struct
  open Ogre.Type

  (** macho segment command *)
  let segment_cmd () =
    Ogre.declare ~name:"segment-command" (scheme name $ off $ size)
      Tuple.T3.create

  let segment_cmd_flags () =
    Ogre.declare ~name:"segment-command-flags"
      (scheme name $ r $ w $ x)
      (fun name r w x -> name, (r,w,x))

  let virtual_segment_cmd () =
    Ogre.declare ~name:"virtual-segment-command"
      (scheme name $ addr $ size) Tuple.T3.create

  (** macho section *)
  let macho_section () =
    Ogre.declare ~name:"macho-section" (scheme name $ addr $ size)
      Tuple.T3.create

  (** macho symbol that doesn't belong to any section *)
  let macho_symbol () =
    Ogre.declare ~name:"macho-symbol" (scheme name $ value)
      Tuple.T2.create

  (** macho symbol defined in some section *)
  let macho_section_symbol () =
    Ogre.declare ~name:"macho-section-symbol" (scheme name $ addr $ size)
      Tuple.T3.create

  (** macho symbol that is a function *)
  let function_ () = Ogre.declare ~name:"function" (scheme addr) ident
end

module Make(Fact : Ogre.S) = struct
  open Image.Scheme
  open Scheme
  open Fact.Syntax

  let segments =
    Fact.foreach Ogre.Query.(
        select (from segment_cmd
                $ segment_cmd_flags
                $ virtual_segment_cmd)
          ~join:[[field name]])
      ~f:(fun (name, offset, size) (_,rwx) (_,addr,vsize)  ->
          name,offset,size,addr,vsize,rwx) >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun (name, off, size, addr, vsize, (r,w,x)) ->
          Fact.provide segment addr vsize r w x >>= fun () ->
          Fact.provide named_region addr vsize name >>= fun () ->
          Fact.provide mapped addr size off)

  let sections =
    Fact.foreach Ogre.Query.(select (from macho_section))
      ~f:ident >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun (name, addr, size) ->
          Fact.provide section addr size >>= fun () ->
          Fact.provide named_region addr size name)

  let symbols =
    Fact.foreach Ogre.Query.(select (from macho_section_symbol))
      ~f:ident >>= fun s ->
    Fact.Seq.iter s ~f:(fun (name, addr, size) ->
        if size = 0L then Fact.return ()
        else
          Fact.provide named_symbol addr name >>= fun () ->
          Fact.provide symbol_chunk addr size addr >>= fun () ->
          Fact.request function_ ~that:(fun a -> a = addr) >>= fun f ->
          if f <> None then
            Fact.provide code_start addr
          else Fact.return ())

  let image =
    segments >>= fun () ->
    sections >>= fun () ->
    symbols

end
