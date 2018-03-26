open Bap.Std
open Bap_rtl_types
open Bap_rtl_bitwidth

module Mem : sig

  module type M = sig
    val mem : var
    val endian : endian
  end

  module Make(M : M) : sig
    val load  : exp -> bitwidth -> exp
    val store : exp -> exp -> bitwidth -> rtl
  end

end

module Reg : sig

  type t

  type name = [
    | `Index of int
    | `Name of string
  ]

  val create : unit ->  t

  val add  : t -> ?aliases:name list -> var -> unit
  val add' : t -> ?aliases:name list -> name -> exp -> unit

  val find : t -> name -> var option
  val find_reg : t -> reg -> exp option
  val find_exp : t -> name -> exp option

  module Exn : sig
    val find : t -> name -> var
    val find_reg : t -> reg -> exp
    val find_exp : t -> name -> exp
  end

end
