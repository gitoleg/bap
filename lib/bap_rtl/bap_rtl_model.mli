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

  type 'a t

  type alias = [
    | `Index of int
    | `Name of string
  ]

  val create : unit -> 'a t
  val add   : 'a t -> ?aliases:alias list -> string -> 'a -> unit

  val add_reg  : var t -> ?aliases:alias list -> string -> int -> unit
  val add_reg' : var t -> ?aliases:alias list -> string -> int -> var

  val exp_of_var : var t -> exp t

  val find  : 'a t -> string -> 'a option
  val find' : 'a t -> reg -> 'a option
  val findi : 'a t -> int -> 'a option
  val chain : ('a t -> 'b -> 'c option) -> 'a t list -> 'b -> 'c option

  val data : 'a t -> 'a list

  module Exn : sig
    val find  : 'a t -> string -> 'a
    val find' : 'a t -> reg -> 'a
    val findi : 'a t -> int -> 'a
    val chain : ('a t -> 'b -> 'c) -> 'a t list -> 'b -> 'c
  end

end
