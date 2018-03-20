open Bap.Std
open Bap_rtl_types
open Bap_rtl_bitwidth

module type M = sig
  val mem : var
  val endian : endian
end

module Make(M : M) : sig
  val load  : exp -> bitwidth -> exp
  val store : exp -> exp -> bitwidth -> rtl
end
