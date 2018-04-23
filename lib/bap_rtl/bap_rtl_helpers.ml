open Core_kernel.Std
open Bap.Std
open Bap_rtl_types

module Exp = Bap_rtl_exp.Exp
open Bap_rtl_bitwidth

let zero = Exp.of_word Word.b0
let one  = Exp.of_word Word.b1
let ones = Exp.pattern Word.b1

let last e bits =
  let w = Exp.width e in
  Exp.extract (w - 1) (w - bits) e

let first e bits = Exp.extract (bits - 1) 0 e

let high w e = last e (int_of_bitwidth w)
let low w e = first e (int_of_bitwidth w)

let msb e =
  let h = Exp.width e - 1 in
  Exp.extract h h e

let lsb e = Exp.extract 0 0 e

let nth w e index =
  let step = int_of_bitwidth w in
  let hi = (index + 1) * step - 1 in
  let lo = index * step in
  Exp.extract hi lo e
