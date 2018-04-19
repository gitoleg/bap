open Core_kernel.Std
open Bap.Std

open Bap_rtl.Std

val cr_bit   : cls
val cr_field : cls

module type Model = sig
  type t

  (** count register  *)
  val ctr : t

  (** link register  *)
  val lr : t

  (** target register  *)
  val tar : t

  (** fixed precision flags *)
  val so : t   (** summary overflow        *)
  val ca : t   (** carry flag              *)
  val ov : t   (** overflow flag           *)
  val ca32 : t (** carry out of 32 bits    *)
  val ov32 : t (** overflow of 32 bits     *)
end

module type PowerPC = sig
  val model : reg_model
  val mem : var
  val gpr_bitwidth : int
  val fpr_bitwidth : int
  val lr_bitwidth  : int
  val ctr_bitwidth : int
  val tar_bitwidth : int
  val cr_bitwidth  : int
  val vr_bitwidth  : int

  include Model with type t := var

  module E  : sig
    include Model with type t := exp

    (** condition register  *)
    val cr  : exp
  end
end

module PowerPC_32 : PowerPC
module PowerPC_64 : PowerPC

module PowerPC_32_cpu : CPU
module PowerPC_64_cpu : CPU
