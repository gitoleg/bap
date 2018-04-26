open Core_kernel.Std
open Bap.Std
open Bap_rtl.Std

module type Model = sig
  type t

  val hi : t
  val lo : t
end

type lexp = lhs exp

module type MIPS = sig
  module E : Model with type t := lexp
  include Model with type t := var

  val model : reg_model
  val mem : var
  val gpr_bitwidth : int
  val fpr_bitwidth : int
end

module MIPS_32 : MIPS
module MIPS_64 : MIPS

module MIPS_32_cpu : CPU
module MIPS_64_cpu : CPU
