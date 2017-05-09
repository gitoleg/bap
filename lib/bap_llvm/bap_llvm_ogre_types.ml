open Core_kernel.Std

module Common_scheme = struct
  open Ogre.Type

  let off  = "offset" %: int
  let size = "size"   %: int
  let name = "name"   %: str
  let addr = "addr"   %: int

  let obj_format () = Ogre.declare ~name:"obj-format" (scheme name) ident
end
