open Core_kernel.Std
open Bap.Std
open Monads.Std

open Image.Scheme
open Bap_llvm_ogre_types.Scheme
open Bap_llvm_elf_scheme

module Make(Fact : Ogre.S) = struct
  open Fact.Syntax

  (** TODO: don't forget to find out why the following doesn't work:
      ~where:(section_flags.(x) = bool(true))
      (fails with strange exception *)
  let sections_as_segments =
    Fact.foreach Ogre.Query.(begin
        select (from section_header $ section_flags)
          ~join:[[field name]]
      end)
      ~f:(fun hdr (_, w, x) -> hdr, (w,x)) >>= fun s ->
    Fact.Fn.ignore @@
    Fact.Seq.fold s ~init:0L
      ~f:(fun addr ((name,_addr,size,off), (w,x)) ->
          let addr = off in
          if x then
            let () =
              printf "segment %s %Lx  %Ld!\n" name addr size in
            Fact.provide segment addr size true w x >>= fun () ->
            Fact.provide mapped addr size off  >>= fun () ->
            Fact.provide named_region addr size name >>= fun () ->
            Fact.return Int64.(addr + size)
          else
            Fact.return Int64.(addr + size))

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

  (** TODO: think about - if it's good enough  *)
  let segments =
    Fact.require is_relocatable >>= fun rel ->
    if rel then sections_as_segments
    else segments

  let sections =
    Fact.foreach Ogre.Query.(select (from section_header))
      ~f:ident >>= fun s ->
      Fact.Seq.iter s
        ~f:(fun (name, addr, size,_) ->
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
          Fact.request code_entry ~that:(fun a -> a = addr) >>= fun f ->
          if f <> None then Fact.provide code_start addr
          else Fact.return ())
end
