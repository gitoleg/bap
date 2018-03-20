open Core_kernel.Std
open Bap.Std

open Bap_rtl_types
open Bap_rtl_utils
open Bap_rtl_bitwidth

let rec bil_exp = function
  | Vars (v, []) -> Bil.var v
  | Vars (v, vars) ->
    List.fold vars ~init:(Bil.var v) ~f:(fun e v -> Bil.(e ^ var v))
  | Word w -> Bil.int w
  | Load (mem, addr, endian, size) ->
    Bil.(load (var mem) (bil_exp addr) endian size)
  | Concat (x, y) -> Bil.(bil_exp x ^ bil_exp y)
  | Binop (op, x, y) -> Bil.binop op (bil_exp x) (bil_exp y)
  | Extract (hi, lo, x) -> Bil.extract hi lo (bil_exp x)
  | Cast (Signed, width, x) -> Bil.(cast signed width (bil_exp x))
  | Cast (Unsigned, width, x) -> Bil.(cast unsigned width (bil_exp x))
  | Unop (op, x) -> Bil.unop op (bil_exp x)

let var_of_exp e = match e.body with
  | Vars (v,_) -> v
  | _ -> failwith "variable expected"

let cast x width sign =
  let nothing_to_cast =
    (x.sign = sign && x.width = width) ||
    (x.width = width && width = 1) in
  if nothing_to_cast then x
  else
  if x.width = 1 then
    {width; sign; body = Cast (x.sign, width, x.body)}
  else
    {width; sign; body = Cast (sign, width, x.body)}

let cast_width x width = cast x width x.sign

let derive_sign s s' = match s, s' with
  | Signed, _ | _, Signed -> Signed
  | _ -> Unsigned

let unop op x = { x with body = Unop (op, x.body)}

let binop_with_signedness sign op lhs rhs =
  let width = max lhs.width rhs.width in
  let lhs = cast lhs width sign in
  let rhs = cast rhs width sign in
  let body = Binop(op, lhs.body, rhs.body) in
  {sign; width; body;}

let unsigned_binop op lhs rhs =
  binop_with_signedness Unsigned op lhs rhs

let signed_binop op lhs rhs =
  binop_with_signedness Signed op lhs rhs

let binop_with_cast op lhs rhs =
  let sign = derive_sign lhs.sign rhs.sign in
  binop_with_signedness sign op lhs rhs

let concat lhs rhs =
  let width = lhs.width + rhs.width in
  let body = Concat (lhs.body, rhs.body) in
  { sign = Unsigned; width; body; }

let bit_result x = cast_width x 1

let derive_op x y op_u op_s =
  match derive_sign x.sign y.sign with
  | Signed -> op_s
  | Unsigned -> op_u

let plus  = binop_with_cast Bil.plus
let minus = binop_with_cast Bil.minus
let times = binop_with_cast Bil.times

let lt x y  =
  let op = derive_op x y Bil.lt Bil.slt in
  bit_result (binop_with_cast op x y)

let gt x y  =
  let op = derive_op x y Bil.lt Bil.slt in
  bit_result (binop_with_cast op y x)

let le x y  =
  let op = derive_op x y Bil.le Bil.sle in
  bit_result (binop_with_cast op x y)

let ge x y  =
  let op = derive_op x y Bil.le Bil.sle in
  bit_result (binop_with_cast op y x)

let divide x y  =
  let op = derive_op x y Bil.divide Bil.sdivide in
  binop_with_cast op x y

let modulo x y =
  let op = derive_op x y Bil.modulo Bil.smodulo in
  binop_with_cast op x y

let eq x y  = bit_result (binop_with_cast Bil.eq x y)
let neq x y = bit_result (binop_with_cast Bil.neq x y)

let lshift = binop_with_cast Bil.lshift
let rshift x y =
  let op =
    if x.sign = Signed then Bil.arshift
    else Bil.rshift in
  binop_with_cast op x y

let bit_and = unsigned_binop Bil.bit_and
let bit_xor = unsigned_binop Bil.bit_xor
let bit_or  = unsigned_binop Bil.bit_or
let not x = unop Bil.not x

module Exp = struct

  let of_var var =
    let width = var_bitwidth var in
    { sign = Unsigned; width; body = Vars (var, []); }

  let of_vars vars = match vars with
    | [] -> failwith "can't constuct an expression from empty var list"
    | v :: vars ->
      let width = width_of_vars (v::vars) in
      { sign = Unsigned; width; body = Vars (v, vars); }

  let of_word w =
    let width = Word.bitwidth w in
    {sign = Unsigned; width; body = Word w }

  let tmp width =
    let var = Var.create ~is_virtual:true ~fresh:true "tmp" (Type.imm width) in
    of_var var

  let load mem addr endian size =
    let width = Size.in_bits size in
    let sign = Unsigned in
    let body = Load (mem,addr.body,endian,size) in
    {sign; width; body;}

  let extract_of_vars e hi lo vars =
    let width = hi - lo + 1 in
    let bounds,_ =
      List.fold ~init:([],0)
        ~f:(fun (acc,n) v ->
            let len = var_bitwidth v in
            let hi = e.width - n - 1 in
            let lo = e.width - n - len in
            (hi, lo, v) :: acc, n + len) vars in
    let bounds = List.rev bounds in
    let has_hi = List.exists ~f:(fun (hi',_,_) -> hi = hi') bounds in
    let has_lo = List.exists ~f:(fun (_,lo',_) -> lo = lo') bounds in
    if has_hi && has_lo then
      let vars = List.filter_map ~f:(fun (hi', lo', v) ->
          if hi' <= hi && lo' >= lo then Some v
          else None) bounds in
      let v = List.hd_exn vars in
      let vars = List.tl_exn vars in
      {sign=Unsigned; width; body = Vars (v,vars)}
    else
      {sign=Unsigned; width; body = Extract (hi,lo,e.body)}

  (* if target width is the same as original expression width, we
     return unsigned original, dependless what sign it had,
     because there is an invariant that extract always returns
     unsigned expression. *)
  let extract hi lo e =
    let width = hi - lo + 1 in
    if width = e.width then { e with sign=Unsigned; }
    else
      match e.body with
      | Vars (v,vars) when vars <> [] ->
        extract_of_vars e hi lo (v :: vars)
      | _ ->
        { sign=Unsigned; width; body = Extract (hi,lo,e.body) }

  let signed e = {e with sign = Signed}
  let unsigned e = {e with sign = Unsigned}
  let width e = e.width
  let body e = e.body
  let sign e = e.sign
  let bil_exp e = bil_exp e.body

  let cast = cast
  let concat = concat

  let width' e =
    let w = width e in
    of_word (Word.of_int ~width:w w)

end

module Infix = struct
  let ( + )  = plus
  let ( - )  = minus
  let ( * )  = times
  let ( / )  = divide
  let ( ^ )  = concat
  let ( % )  = modulo
  let ( < )  = lt
  let ( > )  = gt
  let ( <= )  = le
  let ( >= )  = ge
  let ( = )   = eq
  let ( <> )   = neq
  let ( << )  = lshift
  let ( >> )  = rshift
  let ( lor )  = bit_or
  let ( land ) = bit_and
  let ( lxor ) = bit_xor
  let ( lnot ) = not
end

type 'a ec = bool -> 'a

module Constructor = struct

  let int_of_imm = function
    | Op.Reg _ | Op.Fmm _ -> failwith "imm operand expected"
    | Op.Imm x -> match Imm.to_int x with
      | Some x -> x
      | None -> failwith "failed to convert imm operand to int"

  let imm signed op =
    let w = Word.of_int ~width:32 (int_of_imm op) in
    if signed then Exp.(signed @@ of_word w)
    else Exp.(unsigned @@ of_word w)

  let signed f = f true
  let unsigned f = f false

  let apply_signess signed e =
    if signed then Exp.signed e
    else Exp.unsigned e

  let var signed width =
    Exp.tmp (int_of_bitwidth width) |>
    apply_signess signed

  let reg find signed op = match op with
    | Op.Imm _ | Op.Fmm _ -> failwith "reg operand expected"
    | Op.Reg x -> apply_signess signed (find x)

  let const signed width value =
    let width = int_of_bitwidth width in
    let x = Word.of_int ~width value in
    apply_signess signed (Exp.of_word x)

  let of_string signed s =
    let s = String.filter ~f:(fun c -> c <> '_') s in
    let chop (prefix, multiplier) =
      match String.chop_prefix ~prefix s with
      | None -> None
      | Some data -> Some (multiplier,data) in
    let width,_data =
      match List.find_map ~f:chop ["0x",4; "0o",3; "0b",1;] with
      | Some (mul, data) -> String.length data * mul, data
      | None -> Z.numbits (Z.of_string s), s in
    let suf = if signed then "s" else "u" in
    let w = Word.of_string (sprintf "%s:%d%s" s width suf) in
    apply_signess signed (Exp.of_word w)

end
