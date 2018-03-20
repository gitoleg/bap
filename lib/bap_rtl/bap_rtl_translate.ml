open Core_kernel.Std
open Bap.Std

open Bap_rtl_types
open Bap_rtl_utils
module Exp = Bap_rtl_exp.Exp
module Infix = Bap_rtl_core.Infix
open Exp

let store mem addr data endian size =
  let addr = bil_exp addr in
  let data = bil_exp data in
  Bil.[mem := store (var mem) addr data endian size]

let if_ probe then_ else_ =
  let probe = bil_exp probe in
  Bil.[ if_ probe then_ else_ ]

let rec expand_vars e = match e with
  | Vars (v, vs) -> v :: vs
  | Concat (x,y) ->  expand_vars x @ expand_vars y
  | Cast _ | Extract _ | Load _ | Word _ | Binop _ | Unop _ -> []

let partial_assign v (hi_var, lo_var) rhs (hi_exp, lo_exp) =
  let width = var_bitwidth v in
  let rhs_width = Exp.width rhs in
  let rhs =
    if hi_exp - lo_exp + 1 = rhs_width then rhs
    else Exp.extract hi_exp lo_exp rhs in
  let var = Bil.var v in
  let width_left = width - hi_var - 1 in
  let width_right = lo_var in
  let left =
    if width_left = 0 then None
    else Some (Bil.extract (width - 1) (hi_var + 1) var) in
  let middle = bil_exp rhs in
  let right =
    if width_right = 0 then None
    else Some (Bil.extract (lo_var - 1) 0 var) in
  match left,right with
  | None, None -> Bil.[v := middle]
  | Some left, None -> Bil.[v := left ^ middle]
  | None, Some right -> Bil.[v := middle ^ right]
  | Some left, Some right -> Bil.[ v := left ^ middle ^ right; ]

let assign_vars vars ?hi ?lo rhs =
  let in_bounds x left right = x <= left && x >= right in
  let vars = List.rev vars in
  let full = width_of_vars vars in
  let hi = Option.value ~default:(full - 1) hi in
  let lo = Option.value ~default:0 lo in
  let dif = hi - lo + 1 in
  let rhs = Exp.cast rhs dif rhs.sign in
  let rhs_width = Exp.width rhs in
  let rec assign es assigned vars_lo = function
    | [] -> es
    | v :: vars ->
      let width = var_bitwidth v in
      let vars_hi = width + vars_lo - 1 in
      let var_in_bounds =
        hi < width
        || in_bounds vars_lo hi lo
        || in_bounds vars_hi hi lo in
      if var_in_bounds then
        let lo_var = max lo vars_lo  - vars_lo in
        let hi_var = min (rhs_width + lo - 1) vars_hi - vars_lo in
        let bits_to_assign = hi_var - lo_var + 1 in
        let hi_exp = assigned + bits_to_assign - 1 in
        let lo_exp = assigned in
        let es = partial_assign v (hi_var, lo_var) rhs (hi_exp,lo_exp) @ es in
        assign es (assigned + bits_to_assign) (vars_lo + width) vars
      else assign es assigned (vars_lo + width) vars in
  assign [] 0 0 vars

(** valid forms of assignment:
    1) var := exp
    2) var1 ^ var2 ... varN := exp
    3) [var1;var2; ...] := exp  - equivalent to 2)
    4) extract hi lo var := exp - change only certain bits of var
    5) extract hi lo (var1 ^ var2 ... varN) := exp *)
let rec move lhs rhs =
  match lhs.body with
  | Vars (v, []) ->
    let rhs = Exp.(cast rhs (width lhs) lhs.sign) in
    Bil.[v := bil_exp rhs]
  | Vars (v, vars) -> assign_vars (v::vars) rhs
  | Concat (x,y) ->
    let vars = expand_vars x @ expand_vars y in
    let width = width_of_vars vars in
    let rhs = Exp.cast rhs width rhs.sign in
    assign_vars vars rhs
  | Extract (hi, lo, x) ->
    let vars = expand_vars x in
    assign_vars vars ~hi ~lo rhs
  | _ -> failwith "unexpected left side of :="

let jmp exp = Bil.[ jmp (bil_exp exp)]

class move_finder var =
  object inherit [unit] Stmt.finder
    method! enter_move v _ r =
      if Var.equal var v then r.return (Some ())
      else r
  end

let var_of_exp e = match e.body with
  | Vars (v,_) -> v
  | _ -> failwith "variable expected"

let rec stmt_to_bil = function
  | Move (x,y) -> move x y
  | Jmp a -> jmp a
  | If (cond, then_, else_) -> if_ cond (to_bil then_) (to_bil else_)
  | Message m -> [Bil.special m]
  | Store (mem, addr, data, endian, size) ->
    let bits = Size.in_bits size in
    let data =
      if bits = Exp.width data then data
      else Exp.extract (bits - 1) 0 data in
    store mem addr data endian size
  | Foreach (inverse,step_e, e, code) ->
    let iters = Exp.width e / Exp.width step_e in
    let stepw = Exp.width step_e in
    let has_assignments = has_assignments (var_of_exp step_e) code in
    to_bil @@ List.concat
      (List.init iters
         ~f:(fun i ->
             let i = if inverse then iters - i - 1 else i in
             let hi = (i + 1) * stepw - 1 in
             let lo = i * stepw in
             if has_assignments then
               let last = Infix.(Exp.extract hi lo e := step_e) in
               Infix.(step_e := Exp.extract hi lo e) :: code @ [last]
             else
               Infix.(step_e := Exp.extract hi lo e) :: code))
and to_bil ts =
  List.concat (List.map ~f:stmt_to_bil ts)
and has_assignments var rtl =
  let bil = to_bil rtl in
  Option.is_some ((new move_finder var)#find bil)


let run = to_bil
