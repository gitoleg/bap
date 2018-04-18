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

module Cls : sig
  type t

  val of_string : string -> t

  val gpr : t
  val fpr : t
  val vector : t
  val system : t
  val flag : t
end

type cls = Cls.t

module Reg : sig

  type t

  type name = [
    | `Index of int
    | `Name of string
  ]

  val create  : unit -> t
  val add_reg : t -> cls -> ?aliases:name list -> var -> unit
  val add_exp : t -> cls -> ?aliases:name list -> name -> exp -> unit
  val reg  : t -> ?cls:cls -> name -> var option
  val exp  : t -> ?cls:cls -> name -> exp option

  val reg_exn  : t -> ?cls:cls -> name -> var
  val exp_exn  : t -> ?cls:cls -> name -> exp

  val ec : t -> (op -> exp) ec

end
