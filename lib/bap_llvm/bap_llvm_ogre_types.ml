open Core_kernel.Std

module Scheme = struct
  open Ogre.Type

  let off  = "offset" %: int
  let size = "size"   %: int
  let name = "name"   %: str
  let addr = "addr"   %: int

  let r = "read"    %: bool
  let w = "write"   %: bool
  let x = "execute" %: bool
  let flag = "flag" %: bool

  (** pure symbol's value, without interpretation *)
  let value = "value" %: int

  let file_type () =
    Ogre.declare ~name:"file-type" (scheme name) ident
end

module type S = sig
  type 'a m
  val segments : unit m
  val sections : unit m
  val symbols  : unit m
end

module type Rules = sig
  module Make (F : Ogre.S) : S with type 'a m := 'a F.t
end
