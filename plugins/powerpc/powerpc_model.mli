open Core_kernel.Std
open Bap.Std

open Bap_rtl.Std

module type Model = sig
  type t
  (** all general purpose registers *)
  val gpr  : t reg_model

  (** all floating point registers *)
  val fpr : t reg_model

  (** all vector registers *)
  val vr : t reg_model

  (** count register  *)
  val ctr : t

  (** link register  *)
  val lr : t

  (** target register  *)
  val tar : t

  (** condition register bits *)
  val cr : t reg_model

  (** fixed precision flags *)
  val so : t   (** summary overflow        *)
  val ca : t   (** carry flag              *)
  val ov : t   (** overflow flag           *)
  val ca32 : t (** carry out of 32 bits    *)
  val ov32 : t (** overflow of 32 bits     *)
end

module type Model_exp = sig
  include Model with type t := exp

  (** condition register  *)
  val cr' : exp

  (** condition register fields *)
  val cr_fields  : exp reg_model
end

module type PowerPC = sig
  module E : Model_exp
  include Model with type t := var

  val mem : var
  val flags : Var.Set.t
  val gpr_bitwidth : int
  val fpr_bitwidth : int
  val vr_bitwidth  : int
  val cr_bitwidth  : int
  val lr_bitwidth  : int
  val ctr_bitwidth : int
  val tar_bitwidth : int
end

module PowerPC_32 : PowerPC
module PowerPC_64 : PowerPC

module PowerPC_32_cpu : CPU
module PowerPC_64_cpu : CPU
