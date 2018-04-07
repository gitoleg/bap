open Bap.Std
open Bap_rtl_exp
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

  val create  : unit -> t
  val add_reg : t -> ?aliases:name list -> var -> unit
  val add_exp : t -> ?aliases:name list -> name -> exp -> unit
  val reg  : t -> string -> var option
  val regi : t -> int -> var option
  val exp  : t -> string -> exp option
  val expi : t -> int -> exp option

  module Exn : sig
    val reg  : t -> string -> var
    val exp  : t -> string -> exp
    val regi : t -> int -> var
    val expi : t -> int -> exp
  end

  val reg_ec : t -> (op -> exp) ec

end
