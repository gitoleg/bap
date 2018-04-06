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

    module Lifter = struct

      type 'a t = {
        model : 'a;
        lifts : ('a -> op array -> rtl list) String.Table.t;
      }

      let create model = { model; lifts = String.Table.create () }

      let register {lifts} name lift = Hashtbl.add_exn lifts name lift

      let lifter t =
        let lift _mem insn =
          let insn = Insn.of_basic insn in
          let insn_name = Insn.name insn in
          match Hashtbl.find t.lifts insn_name with
          | None ->  Or_error.errorf "unknown instruction %s" insn_name
          | Some lift ->
            try
              lift t.model (Insn.ops insn) |>
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
              Error (Error.of_string str) in
        lift
    end
  end

  type reg_model = Model.Reg.t
  type 'a lift_model = 'a Model.Lifter.t

end
