open Core_kernel.Std
open Bap.Std
open Bap_rtl_types
open Bap_rtl_core

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

let is_assignable e = match e.body with
  | Vars _ -> true
  | _ -> false

let has_assignments e rtl =
  let is_same e' = match e.body, e'.body with
    | Vars (v,[]), Vars (v', []) -> Var.equal v v'
    | _ -> false in
  let rec loop = function
    | [] -> false
    | Move (e',_) :: _ when is_same e' -> true
    | If (_, yes, no)  :: rtl -> (loop yes || loop no) || loop rtl
    | Block sts :: rtl -> loop sts || loop rtl
    | _ :: rtl -> loop rtl in
  loop rtl

let foreach_base ~inverse ~inject_assign (step_e : lhs exp) e code =
  let iters = Exp.width e / Exp.width step_e in
  let stepw = Exp.width step_e in
  let add_assign = inject_assign && has_assignments step_e code in
  let rtl = List.init iters
      ~f:(fun i ->
          let i = if inverse then iters - i - 1 else i in
          let hi = (i + 1) * stepw - 1 in
          let lo = i * stepw in
          if add_assign then
            let last = Infix.(Exp.extract hi lo e := step_e) in
            Infix.(step_e := Exp.extract hi lo e) :: code @ [last]
          else
            Infix.(step_e := Exp.extract hi lo e) :: code) in
  Block (List.concat rtl)

let foreach = foreach_base ~inverse:false ~inject_assign:false
let foreach_rev = foreach_base ~inverse:true ~inject_assign:false

let foreach' = foreach_base ~inverse:false ~inject_assign:true
let foreach_rev' = foreach_base ~inverse:true ~inject_assign:true
