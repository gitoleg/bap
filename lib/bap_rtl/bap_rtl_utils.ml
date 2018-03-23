open Core_kernel.Std
open Bap.Std

let var_bitwidth v =
  match Var.typ v with
  | Type.Imm w -> w
  | _ ->
    failwith @@
    sprintf "variable %s doesn't has notion of bitwidth" (Var.name v)

let width_of_vars vs =
  List.fold ~init:0 ~f:(fun x v -> x + var_bitwidth v) vs
