open Bap.Std
open Bap_rtl_types

module Rtl : sig

  val store : var -> exp -> exp -> endian -> size -> rtl
  val if_ : exp -> rtl list -> rtl list -> rtl
  val jmp : exp -> rtl

  (** [foreach step e code] - repeats [code] for each
      [step] of [e]. *)
  val foreach : exp -> exp -> rtl list -> rtl

  (** [foreach' step e code] - the same as above, but starts iteration
      from the most significant bits of [e]. *)
  val foreach' : exp -> exp -> rtl list -> rtl

  val message : string -> rtl

  val when_ : exp -> rtl list -> rtl
  val ifnot : exp -> rtl list -> rtl

  type clause

  val switch  : exp -> clause list -> rtl
  val case    : exp -> rtl list -> clause
  val default : rtl list -> clause

end

module Infix : sig
  val ( := )  : exp -> exp -> rtl
end
