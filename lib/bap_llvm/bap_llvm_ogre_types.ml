open Core_kernel.Std

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
