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

(** [remove_forward_defs bil defs]

    for every [x := exp] in [bil], where [exp] depends on some
    [y1 .. yN] redefined later then [x],
    removes both [x] and [y1 .. yN] from defs *)
let remove_forward_defs bil defs =
  (object
    inherit [exp Var.Map.t] Stmt.visitor
    method! enter_move var e defs =
      let redefined = Set.filter (Exp.free_vars e)
          ~f:(fun v -> Bil.is_assigned v succs) in
      if Set.is_empty redefined then defs
      else
        Set.fold ~init:(Map.remove defs var) redefined
          ~f:(fun defs v ->
              if Bil.is_assigned v succs then Map.remove defs v
              else defs)
  end)#run bil defs

let substitute defs e =
  let mapper defs = object
    inherit Stmt.mapper as super
    method! map_var v = match Map.find defs v with
      | Some e -> e
      | None -> Bil.var v
  end in
  Exp.fold_consts @@ (mapper defs)#map_exp e

let propagate_consts bil =
  let remove_unequal defs defs' =
    let diff = Map.symmetric_diff defs defs' ~data_equal:Exp.equal in
    Seq.fold diff ~init:defs ~f:(fun defs (v,diff) -> match diff with
        | `Unequal _ -> Map.remove defs v
        | _ -> defs) in
  let rec run ss defs = function
    | [] -> List.rev ss, defs
    | Bil.Move (v,e) :: bil ->
      let defs, e = match substitute defs e with
        | Bil.Int _ as e -> Map.add defs v e, e
        | e -> Map.remove defs v, e in
      run (Bil.move v e :: ss) defs bil
    | Bil.If (cond,yes,no) :: bil ->
      let yes,defs_yes = run [] defs yes in
      let no, defs_no  = run [] defs no  in
      let cond = substitute defs cond in
      let defs' = match cond with
        | Bil.Int w when Word.is_one  w -> defs_yes
        | Bil.Int w when Word.is_zero w -> defs_no
        | _ ->
          let defs = remove_unequal defs defs_yes in
          remove_unequal defs defs_no in
      run (Bil.if_ cond yes no :: ss) defs' bil
    | Bil.While (cond,body) :: bil ->
      let defs = remove_forward_defs body defs in
      let body, defs' = run [] defs body in
      let cond = substitute defs cond in
      let defs' = match cond with
        | Bil.Int w when Word.is_one w -> defs'
        | _ -> remove_unequal defs defs' in
      run (Bil.while_ cond body :: ss) defs' bil
    | Bil.Jmp dst :: bil ->
      let dst = substitute defs dst in
      run (Bil.jmp dst :: ss) defs bil
    | s :: bil -> run (s :: ss) defs bil in
  fst @@ run [] Var.Map.empty bil


let bil_of_rtl rtl = Translate.run rtl |> propagate_consts

module Lifter_model = struct

  module type Cpu = sig
    type t
  end

  module Array = struct
    type 'a t = 'a Array.t

    exception Invalid_operand_index of int

    let get a n =
      if n >= Array.length a then raise (Invalid_operand_index n)
      else Array.get a n

    let unsafe_get a n = get a n
  end

  module Make (T : Cpu) = struct
    open Bap_rtl_types

    let lifts : (T.t -> op array -> rtl list) String.Table.t = String.Table.create ()

    let register name lift =
      match Hashtbl.add lifts name lift with
      | `Ok -> ()
      | `Duplicate ->
        warning
          "trying to register a %s instruction, that already exists"
          name

    let lift model _mem insn =
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
