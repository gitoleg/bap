open Core_kernel.Std
open Bap.Std

module Std = struct

  open Bap_rtl_kernel

  include Bap_rtl_helpers

  module RTL = struct
    include Bap_rtl_kernel
    include Infix
  end

  type rtl = RTL.rtl [@@deriving bin_io, compare, sexp]
  type exp = RTL.exp [@@deriving bin_io, compare, sexp]
  let bil_of_rtl = RTL.bil_of_t

  module Array = Op_array

  (* let concat f g = fun cpu ops -> f cpu ops @ g cpu ops *)

  (* let (^) = concat *)

  (* let lifters = String.Table.create () *)

  (* let register name lifter = *)
  (*   Hashtbl.change lifters name ~f:(fun _ -> Some lifter) *)

  (* let (>|) = register *)

  (* let lift addr_size endian mem insn = *)
  (*   let insn = Insn.of_basic insn in *)
  (*   let insn_name = Insn.name insn in *)
  (*   let cpu = make_cpu addr_size endian mem  in *)
  (*   let lift lifter = *)
  (*     try *)
  (*       lifter cpu (Insn.ops insn) |> *)
  (*       bil_of_rtl |> *)
  (*       Result.return *)
  (*     with *)
  (*     | Failure str -> Error (Error.of_string str) in *)
  (*   match Hashtbl.find lifters (Insn.name insn) with *)
  (*   | None -> Or_error.errorf "unknown instruction %s" insn_name *)
  (*   | Some lifter -> lift lifter *)

end
