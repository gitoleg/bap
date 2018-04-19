open Bap.Std
open Bap_rtl_exp
open Bap_rtl_types
open Bap_rtl_bitwidth

module Mem : sig
  type load = exp -> bitwidth -> exp
  type store = exp -> exp -> bitwidth -> rtl

  val load  : var -> endian -> load
  val store : var -> endian -> store
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

  val create  : unit -> t
  val add  : t -> cls -> ?aliases:name list -> var -> unit
  val find  : t -> ?cls:cls -> name -> var option
  val find_exn  : t -> ?cls:cls -> name -> var
  val ec : t -> (op -> exp) ec
  val all  : t -> cls -> var list

  module Exp : sig
    val add : t -> cls -> ?aliases:name list -> name -> exp -> unit
    val find  : t -> ?cls:cls -> name -> exp option
    val find_exn : t -> ?cls:cls -> name -> exp
    val all : t -> cls -> exp list
  end

end
