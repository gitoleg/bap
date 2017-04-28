open Core_kernel.Std
open Monads.Std

module Fact = Ogre.Make(Monad.Ident)
module Result = Monad.Result.Error
open Result.Syntax

module Elf_scheme = struct
  open Ogre.Type

  type 'a loc = {off : int64; size : int64; data : 'a; }

  let p_off  = "p_off"   %: int
  let p_size = "p_size"  %: int
  let p_addr = "p_addr"  %: int
  let v_off  = "v_off"   %: int
  let v_size = "v_size"  %: int
  let v_addr = "v_addr"  %: int
  let name = "name"  %: str

  let hdr_number = "header_number" %: int
  let hdr_type   = "header_type"   %: int
  let hdr_flags  = "header_flags"  %: int
  let align     = "aligin" %: int

  let declare name scheme f = Ogre.declare ~name scheme f

  let header () = declare "elf-header"
      (scheme p_off $ p_size $ hdr_type $ hdr_flags $ align)
      (fun off size typ flags align ->
         {off; size; data = (typ, flags, align)})

  let physical () = declare "physical-view"
      (scheme p_off $ p_addr $ p_size)
      (fun off addr size -> {off; size; data = addr;})

  let virtual_ () = declare "virtual-view"
      (scheme p_off $ p_size $ v_addr $ v_size)
      (fun poff psize vaddr vsize ->
         {off = poff; size = psize; data = (vaddr, vsize);})

  let hdr_number () = declare "header-number"
      (scheme p_off $ p_size $ hdr_number)
      (fun off size num -> {off; size; data = num;})


end

let is_r flags = Int64.(bit_and flags (of_int 4) = one)
let is_w flags = Int64.(bit_and flags (of_int 2) = one)
let is_x flags = Int64.(bit_and flags (of_int 1) = one)

module Image_ogre_of_elf = struct
  open Bap_image.Scheme
  open Elf_scheme

  let segments =
    Fact.foreach Ogre.Query.(begin
        select
          (from header $ virtual_)
          ~join:([[field p_off]; [field p_size]])
      end)
      ~f:(fun {off=p_off; size=p_size; data=(_,flags,_)} {data=(v_addr, v_size)} ->
          Fact.provide segment
            v_addr v_size (is_r flags) (is_w flags) (is_x flags))

end
