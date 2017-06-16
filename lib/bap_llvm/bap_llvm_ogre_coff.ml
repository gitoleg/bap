open Core_kernel.Std
open Bap.Std

open Bap_llvm_ogre_types.Scheme
open Image.Scheme
open Bap_llvm_coff_scheme

module Make(Fact : Ogre.S) = struct
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
    Fact.collect Ogre.Query.(select (from symbol)) >>= fun s ->
    Fact.Seq.iter s ~f:(fun (name, addr, size) ->
        if size = 0L then Fact.return ()
        else
          Fact.provide named_symbol addr name >>= fun () ->
          Fact.provide symbol_chunk addr size addr >>= fun () ->
          Fact.request function_
            ~that:(fun (a,n) -> a = addr && n = name) >>= fun f ->
          if f <> None then Fact.provide code_start addr
          else Fact.return ())
end
