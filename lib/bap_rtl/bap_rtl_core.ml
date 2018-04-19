open Core_kernel.Std
open Bap.Std
open Bap_rtl_types

module Rtl = struct
  let store mem addr x endian size = Store (mem, addr, x, endian, size)
  let jmp addr = Jmp addr
  let move x y = Move (x,y)
  let if_ cond then_ else_ = If (cond, then_, else_)
  let foreach  step exp code = Foreach (false,step,exp,code)
  let foreach' step exp code = Foreach (true,step,exp,code)
  let message m = Message m

  let when_ cond then_ = if_ cond then_ []
  let ifnot cond else_ = if_ cond [] else_

  type clause = [
    | `Case of (exp * rtl list)
    | `Default of rtl list
  ]

  let case x y = `Case (x,y)
  let default y = `Default y

  let switch exp cases =
    let default = List.filter_map ~f:(function
        | `Default y -> Some y
        |  _ -> None) cases in
    let default = Option.value ~default:[] (List.hd default) in
    let cases = List.filter_map ~f:(function
        | `Case (x,y) -> Some (x,y)
        | _ -> None) cases in
    let cond x = Bap_rtl_exp.Infix.(exp = x) in
    match cases with
    | [] -> failwith "empty switch"
    | (x, code) :: [] -> (if_ (cond x) code default;)
    | (x, code) :: cases ->
      let else_ =
        List.fold (List.rev cases) ~init:default ~f:(fun acc (x,code) ->
            [if_ (cond x) code acc;]) in
      (if_ (cond x) code else_)

end

module Infix = struct
  let ( := )  = Rtl.move
end