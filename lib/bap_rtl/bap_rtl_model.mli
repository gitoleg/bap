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

  val create  : unit -> t
  val add_reg : t -> cls -> ?aliases:name list -> var -> unit
  val add_reg' : t -> cls -> ?aliases:name list -> name -> exp -> unit
  val reg  : t -> ?cls:cls -> string -> var option
  val exp  : t -> ?cls:cls -> string -> exp option

  val regi : t -> cls -> int -> var option
  val expi : t -> cls -> int -> exp option

  module Exn : sig
    val reg  : t -> ?cls:cls -> string -> var
    val exp  : t -> ?cls:cls -> string -> exp
    val regi : t -> cls -> int -> var
    val expi : t -> cls -> int -> exp
  end

  val reg_ec : t -> (op -> exp) ec

end
