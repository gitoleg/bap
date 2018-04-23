open Core_kernel.Std
open Bap.Std

include Self ()

module Translate = Bap_rtl_translate
module Helpers = Bap_rtl_helpers
module Bitwidth = Bap_rtl_bitwidth

module Infix = struct
  include Bap_rtl_exp.Infix
  include Bap_rtl_core.Infix
end

module RTL = struct
  let extract = Bap_rtl_exp.Exp.extract
  include Bap_rtl_core.Rtl
  include Infix
  include Helpers
end

module Array = struct
  type 'a t = 'a Array.t

  exception Invalid_operand_index of int

  let get a n =
    if n >= Array.length a then raise (Invalid_operand_index n)
    else Array.get a n

  let unsafe_get a n = get a n
end

let propagate_consts bil =
  let mapper known = object
    inherit Stmt.mapper as super
    method! map_var v = match Map.find known v with
      | Some e -> e
      | None -> Bil.var v
  end in
  let subst known e =
    Exp.fold_consts @@ (mapper known)#map_exp e in
  let diff known known' =
    let diff  = Map.symmetric_diff known known' ~data_equal:Exp.equal in
    Seq.fold diff ~init:known ~f:(fun known (v,diff) -> match diff with
        | `Unequal _ -> Map.remove known v
        | _ -> known) in
  let rec run acc known = function
    | [] -> List.rev acc,known
    | Bil.Move (v,e) :: bil ->
      let e = subst known e in
      let known = Map.add known v e in
      run (Bil.move v e :: acc) known bil
    | Bil.If (cond,yes,no) :: bil ->
      let yes,known_yes = run [] known yes in
      let no,known_no  = run [] known no in
      let cond, known' = match subst known cond with
        | Bil.Int w as e when Word.(equal w b1) -> e, known_yes
        | Bil.Int w as e when Word.(equal w b0) -> e, known_no
        | e ->
          let known = diff known known_yes in
          let known = diff known known_no in
          e, known in
      run (Bil.if_ cond yes no :: acc) known' bil
    | Bil.While (cond,body) :: bil ->
      let body,known' = run [] known body in
      let cond, known' = match subst known cond with
        | Bil.Int w as e when Word.(equal w b1) -> e, known'
        | cond -> cond, diff known known' in
      run (Bil.while_ cond body :: acc) known' bil
    | Bil.Jmp dst :: bil ->
      let dst = subst known dst in
      run (Bil.jmp dst :: acc) known bil
    | s :: bil -> run (s :: acc) known bil in
  let bil',_ = run [] Var.Map.empty bil in
  bil'

let bil_of_rtl rtl=
  Translate.run rtl |>
  propagate_consts

module Lifter_model = struct

  module type Cpu = sig
    type t
    val update : t -> addr -> t
  end

  module Make (T : Cpu) = struct
    let lifts = String.Table.create ()
    let model : T.t option ref  = ref None

    let update_model mem =
      match !model with
      | None -> ()
      | Some m ->
        model := Some (T.update m (Memory.min_addr mem))

    let init m = model := Some m

    let register name lift =
      match Hashtbl.add lifts name lift with
      | `Ok -> ()
      | `Duplicate ->
        warning
          "trying to register a %s instruction, that already exists"
          name

    let lifter mem insn =
      match !model with
      | None -> failwith "trying to use uninitialized lifter"
      | Some model ->
        update_model mem;
        let insn = Insn.of_basic insn in
        let insn_name = Insn.name insn in
        match Hashtbl.find lifts insn_name with
        | None ->  Or_error.errorf "unknown instruction %s" insn_name
        | Some lift ->
          try
            lift model (Insn.ops insn) |>
            bil_of_rtl |>
            Result.return
          with
          | Array.Invalid_operand_index n ->
            let str =
              sprintf "instruction %s doesn't have an operand with index %d"
                insn_name n in
            Error (Error.of_string str)
          | exn ->
            let str = Exn.to_string exn in
            Error (Error.of_string str)
  end
end

module Std = struct

  include Bap_rtl_types

  type cls = Bap_rtl_model.cls [@@deriving bin_io,compare,sexp]
  type reg_model = Bap_rtl_model.Reg.t
  type 'a ec = 'a Bap_rtl_exp.ec

  module RTL = RTL
  module Exp = Bap_rtl_exp.Exp
  module Bitwidth = Bap_rtl_bitwidth
  module Ec = Bap_rtl_exp.Constructor
  module Cls = Bap_rtl_model.Cls
  module Mem_model = Bap_rtl_model.Mem
  module Reg_model = Bap_rtl_model.Reg
  module Lifter_model = Lifter_model
  module Array = Array

  let bil_of_rtl = bil_of_rtl

end
