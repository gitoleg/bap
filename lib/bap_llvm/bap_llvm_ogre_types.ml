open Core_kernel.Std

module Common_fields = struct
  open Ogre.Type

  let off  = "offset" %: int
  let size = "size"   %: int
  let name = "name"   %: str
  let addr = "addr"   %: int

  let r = "read"    %: bool
  let w = "write"   %: bool
  let x = "execute" %: bool
  let flag = "flag" %: bool

end
