open Core_kernel.Std
open Bap.Std
open Monads.Std

open Image.Scheme
open Bap_llvm_ogre_types.Scheme
open Bap_llvm_elf_scheme

module Make(Fact : Ogre.S) = struct
  open Fact.Syntax

  let segments =
    Fact.foreach Ogre.Query.(begin
        select (from
                  program_header
                $ virtual_program_header
                $ program_header_flags)
          ~join:[[field name]]
      end)
      ~f:(fun hdr vhdr hdr_flags -> hdr, vhdr, hdr_flags) >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun ((name,off,size), (_,addr, vsize), (_,ld,r,w,x)) ->
          if ld then
            Fact.provide segment addr vsize r w x >>= fun () ->
            Fact.provide mapped addr size off >>= fun () ->
            Fact.provide named_region addr vsize name
          else Fact.return ()) >>= Fact.return

  let sections =
    Fact.collect Ogre.Query.(select (from section_header)) >>= fun s ->
      Fact.Seq.iter s
        ~f:(fun (name, addr, size, _) ->
            Fact.provide section addr size >>= fun () ->
            Fact.provide named_region addr size name)

  let symbols =
    Fact.collect Ogre.Query.(select (from symbol_entry)) >>= fun s ->
    Fact.Seq.iter s ~f:(fun (name, addr, size) ->
        if size = 0L then Fact.return ()
        else
          Fact.provide named_symbol addr name >>= fun () ->
          Fact.provide symbol_chunk addr size addr >>= fun () ->
          Fact.request code_entry ~that:(fun (a,_) -> a = addr) >>= fun f ->
          if f <> None then Fact.provide code_start addr
          else Fact.return ())

end

module Relocatable = struct
  module Make(Fact : Ogre.S) = struct
    open Fact.Syntax

    (** TODO: provide an artificial entry *)
    let entry = 0x0L

    (** TODO: don't forget to find out why the following doesn't work:
        ~where:(section_flags.(x) = bool(true))
        (fails with strange exception *)
    let segments =
      Fact.foreach Ogre.Query.(begin
          select (from section_header $ section_flags)
            ~join:[[field name]]
        end)
        ~f:(fun hdr (_, w, x) -> hdr, (w,x)) >>= fun s ->
      Fact.Seq.iter s
        ~f:(fun ((name,_,size,off), (w,x)) ->
            let addr = Int64.(entry + off) in
            if x then
              Fact.provide segment addr size true w x >>= fun () ->
              Fact.provide mapped addr size off  >>= fun () ->
              Fact.provide named_region addr size name
            else Fact.return ())

    let symbols =
      Fact.collect Ogre.Query.(select (from symbol_entry)) >>= fun s ->
      Fact.Seq.iter s ~f:(fun (name, addr, size) ->
          if size = 0L then Fact.return ()
          else
            Fact.provide named_symbol addr name >>= fun () ->
            Fact.provide symbol_chunk addr size addr >>= fun () ->
            Fact.request code_entry
              ~that:(fun (a,n) -> a = addr && n = name) >>= fun f ->
            if f <> None then Fact.provide code_start addr
            else Fact.return ())

    let sections =
      Fact.collect Ogre.Query.(select (from section_header)) >>= fun s ->
      Fact.Seq.iter s
        ~f:(fun (name, _, size, off) ->
            let addr = Int64.(entry + off) in
            Fact.provide section addr size >>= fun () ->
            Fact.provide named_region addr size name)
  end
end
