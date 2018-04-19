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

module Std = struct

  include Bap_rtl_types

  module Exp = Bap_rtl_exp.Exp

  module RTL = struct
    let extract = Exp.extract
    include Bap_rtl_core.Rtl
    include Infix
    include Helpers
  end

  type 'a ec = 'a Bap_rtl_exp.ec

  module Bitwidth = Bap_rtl_bitwidth
  module Ec = Bap_rtl_exp.Constructor

  let bil_of_rtl rtl =
    Helpers.norm_jumps @@
    Translate.run rtl

  module Cls = Bap_rtl_model.Cls
  module Mem_model = Bap_rtl_model.Mem
  module Reg_model = Bap_rtl_model.Reg

  type cls = Bap_rtl_model.cls [@@deriving bin_io,compare,sexp]
  type reg_model = Reg_model.t

  module Array = struct

    type 'a t = 'a Array.t

    exception Invalid_operand_index of int

    let get a n =
      if n >= Array.length a then raise (Invalid_operand_index n)
      else Array.get a n

    let unsafe_get a n = get a n
  end

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
end
