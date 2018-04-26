open Core_kernel.Std
open Bap.Std
open Bap_rtl.Std
open Bitwidth
open Mips_utils
open Mips_types
open Mips_model

let make_cpu addr_size endian memory =
  let (module M) = match addr_size with
    | `r32 -> (module MIPS_32 : MIPS)
    | `r64 -> (module MIPS_64) in
  let open M.E in
  let reg = Reg_model.ec M.model in
  let load e b = Mem_model.load M.mem endian e b in
  let store a d w = Mem_model.store M.mem endian a d w in
  let cia = Memory.min_addr memory |>
            Exp.of_word |>
            Exp.signed in
  let jmp e = match addr_size with
    | `r32 -> RTL.(jmp (low word e))
    | `r64 -> RTL.jmp e in
  let find cls n =
    match Reg_model.Exp.find M.model (`Index n) ~cls with
    | None ->
      mips_fail "%s with number %d not found"
        (Sexp.to_string (sexp_of_cls cls)) n
    | Some x -> x in
  let word_width, word_bitwidth = match addr_size with
    | `r32 -> Ec.(unsigned const byte 32), word
    | `r64 -> Ec.(unsigned const byte 64), doubleword in
  { load; store; jmp; cia; word_width; word_bitwidth;
    reg; hi; lo;
    gpr = find Cls.gpr;
    fpr = find Cls.fpr;
  }
