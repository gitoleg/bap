open Bap.Std
open Bap_rtl_exp
open Bap_rtl_types
open Bap_rtl_bitwidth

module Mem : sig
  type 'a load = 'a exp -> bitwidth -> rhs exp
  type ('a, 'b) store = 'a exp -> 'b exp -> bitwidth -> rtl

  val load  : var -> endian -> 'a load
  val store : var -> endian -> ('a, 'b) store
end

type cls [@@deriving bin_io, compare, sexp]

module Cls : sig
  type t = cls

  val of_string : string -> t

  val gpr : t
  val fpr : t
  val vector : t
  val system : t
  val flag : t
end

module Reg : sig

  type t

  type name = [
    | `Index of int
    | `Name of string
  ]

  val empty : t
  val add   : cls -> ?aliases:name list -> var -> t -> t
  val find  : t -> ?cls:cls -> name -> var option
  val find_exn  : t -> ?cls:cls -> name -> var
  val ec : t -> (op -> lhs exp) ec
  val all  : t -> cls -> var list

  module Exp : sig
    val add : cls -> ?aliases:name list -> name -> lhs exp -> t -> t
    val find  : t -> ?cls:cls -> name -> lhs exp option
    val find_exn : t -> ?cls:cls -> name -> lhs exp
    val all : t -> cls -> lhs exp list
  end

end
