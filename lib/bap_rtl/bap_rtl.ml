open Core_kernel.Std
open Bap.Std

module Translate = Bap_rtl_translate
module Helpers = Bap_rtl_helpers
module Exp_constructor = Bap_rtl_exp.Constructor
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
  end

  type 'a ec = 'a Bap_rtl_exp.ec

  include Exp_constructor
  include Helpers
  include Bitwidth

  let bil_of_rtl rtl =
    Helpers.norm_jumps @@
    Translate.run rtl

  module Model = struct

    include Bap_rtl_model

    module Array = struct

      type 'a t = 'a Array.t

      exception Invalid_operand_index of int

      let get a n =
        if n >= Array.length a then raise (Invalid_operand_index n)
        else Array.get a n

      let unsafe_get a n = get a n
    end

    module type Cpu = sig
      type t
      val update : t -> addr -> t
    end

    module Lifter (T : Cpu) = struct
      let lifts = String.Table.create ()
      let model : T.t option ref  = ref None

      let update_model mem =
        match !model with
        | None -> ()
        | Some m ->
          model := Some (T.update m (Memory.min_addr mem))

      let init m = model := Some m

      let register name lift = Hashtbl.add_exn lifts name lift

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

  type reg_model = Model.Reg.t

end
