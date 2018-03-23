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
  val find  : 'a t -> string -> 'a
  val find' : 'a t -> reg -> 'a
  val findi : 'a t -> int -> 'a

  module Var : sig
    val add  : var t -> ?aliases:alias list -> string -> int -> unit
    val add' : var t -> ?aliases:alias list -> string -> int -> var
  end

  module Exp : sig
    val of_var : var t -> exp t
  end
end
