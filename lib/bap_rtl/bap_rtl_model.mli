open Bap.Std
open Bap_rtl_kernel
open Bap_rtl_helpers

module Model : sig

  module type M = sig
    val mem : var
    val endian : endian
  end

  module Reg_class : sig
    type t [@@deriving bin_io,compare,sexp]

    val create : string -> t

    val gpr : t
    val fpr : t
    val flag : t
    val vector : t

  end

  type cls = Reg_class.t [@@deriving bin_io,compare,sexp]
  exception Register_not_found of string

  module Make(M : M) : sig
    val load  : exp -> bitwidth -> exp
    val store : exp -> exp -> bitwidth -> rtl

    val add_var :          var -> ?alises:string list -> ?index:int -> cls -> unit
    val add_reg : string -> int -> ?alises:string list -> ?index:int -> cls -> unit
    val add_exp : string -> exp -> ?alises:string list -> ?index:int -> cls -> unit

    val find  : cls -> reg -> exp
    val findi : cls -> int -> exp
  end
end
