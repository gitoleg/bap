open Bap.Std
open Bap_rtl_types

module Rtl : sig

  val move : lhs exp -> 'a exp -> rtl
  val store : var -> 'a exp -> 'b exp -> endian -> size -> rtl
  val if_ : 'a exp -> rtl list -> rtl list -> rtl
  val jmp : 'a exp -> rtl


  val foreach : lhs exp -> 'a exp -> rtl list -> rtl
  val foreach_rev : lhs exp -> 'a exp -> rtl list -> rtl

  val foreach' : lhs exp -> lhs exp -> rtl list -> rtl
  val foreach_rev' : lhs exp -> lhs exp -> rtl list -> rtl


  val message : string -> rtl

  val when_ : 'a exp -> rtl list -> rtl
  val ifnot : 'a exp -> rtl list -> rtl

  type clause

  val switch  : 'a exp -> clause list -> rtl
  val case    : 'a exp -> rtl list -> clause
  val default : rtl list -> clause

end

module Infix : sig
  val ( := )  : lhs exp -> 'a exp -> rtl
end
