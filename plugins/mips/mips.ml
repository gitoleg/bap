open Core_kernel.Std
open Bap.Std

(* This CPU model and instruction set is based on the
 * "MIPS Architecture For Programmers
 * Volume II-A: The MIPS64 Instruction Set Reference Manual"
 * Document Number: MD00087  Revision 6.04
 * November 13, 2015 *)

module Model = Mips_model

module Zero_reg_elimination = struct

  class map_reads reg0 = object
    inherit Stmt.mapper as super

    method! map_var v =
      if Var.equal reg0 v then
        match Var.typ v with
        | Type.Imm w -> Bil.Int (Word.zero w)
        | _ -> failwith "unexpected variable type"
      else super#map_var v
  end

  let remove_writes reg0 bil =
    let rec map = function
      | Bil.Move (v, e) as mv ->
        if Var.equal reg0 v then None
        else Some mv
      | Bil.If (e, then_, else_) ->
        let then_ = List.filter_map ~f:map then_ in
        let else_ = List.filter_map ~f:map else_ in
        Some (Bil.If (e, then_, else_))
      | While (e, body) ->
        Some (While (e, List.filter_map ~f:map body))
      | s ->  Some s in
    List.filter_map bil ~f:map

  let remove_reads is_reg0 bil =
    (new map_reads is_reg0)#run bil

  let run reg0 bil =
    remove_writes reg0 bil |>
    remove_reads reg0

end


module Std = struct
  open Mips_rtl
  include Mips_utils
  include Mips_types
  include Mips_cpu
  include Mips_dsl

  module RTL = struct
    include Mips_rtl
    include Infix
    let foreach = foreach ~inverse:true
  end

  type rtl = RTL.rtl [@@deriving bin_io, compare, sexp]
  type exp = RTL.exp [@@deriving bin_io, compare, sexp]
  type lift = cpu -> op array -> rtl list

  let bil_of_rtl = RTL.bil_of_t

  let concat f g = fun cpu ops -> f cpu ops @ g cpu ops

  let (^) = concat

  let lifters = String.Table.create ()

  let register name lifter =
    String.Table.change lifters name ~f:(fun _ -> Some lifter)

  let (>>) = register

  let lift addr_size endian reg0 mem insn =
    let insn = Insn.of_basic insn in
    let insn_name = Insn.name insn in
    let cpu = make_cpu addr_size endian mem in
    let lift lifter =
      try
        lifter cpu (Insn.ops insn) |>
        bil_of_rtl |>
        Zero_reg_elimination.run reg0 |>
        Result.return
      with
      | Failure str -> Error (Error.of_string str) in
    match String.Table.find lifters (Insn.name insn) with
    | None -> Or_error.errorf "unknown instruction %s" insn_name
    | Some lifter -> lift lifter

  let get_reg0 m =
    let module M = (val m : Model.MIPS) in
    Map.find_exn M.gpri 0

  let make_lift m =
    let module M = (val m : Model.MIPS) in
    let reg0 = Map.find_exn M.gpri 0 in
    fun addr_size endian -> lift addr_size endian reg0

  module M32BE = struct
    module CPU = Model.MIPS_32_cpu

    let lift = make_lift (module Model.MIPS_32) `r32 BigEndian
  end

  module M32LE = struct
    module CPU = Model.MIPS_32_cpu

    let lift = make_lift (module Model.MIPS_32) `r32 LittleEndian
  end

  module M64BE = struct
    module CPU = Model.MIPS_64_cpu

    let lift = make_lift (module Model.MIPS_64) `r64 BigEndian
  end

  module M64LE = struct
    module CPU = Model.MIPS_64_cpu

    let lift = make_lift (module Model.MIPS_64) `r64 LittleEndian
  end

  include Model

end
