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





(* module Op_array = struct *)

(*   type 'a t = 'a Array.t *)

(*   exception Invalid_operand_index of int *)

(*   let get a n = *)
(*     if n >= Array.length a then raise (Invalid_operand_index n) *)
(*     else Array.get a n *)

(*   let unsafe_get a n = get a n *)
(* end *)

(* module Op_array : sig *)

(*   type 'a t = 'a Array.t *)

(*   exception Invalid_operand_index of int *)

(*   val get : 'a t -> int -> 'a *)
(*   val unsafe_get : 'a t -> int -> 'a *)
(* end *)
