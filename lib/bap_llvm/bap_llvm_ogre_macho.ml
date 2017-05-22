open Core_kernel.Std
open Bap.Std
open Bap_llvm_ogre_types.Scheme
open Bap_llvm_macho_scheme
open Image.Scheme

module Make(Fact : Ogre.S) = struct
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
end
