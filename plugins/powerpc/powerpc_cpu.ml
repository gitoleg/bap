open Core_kernel.Std
open Bap.Std

open Bap_rtl.Std
open Bitwidth

open Powerpc_utils
open Powerpc_model
open Powerpc_types

let make_cpu addr_size endian memory =
  let (module M) = match addr_size with
    | `r32 -> (module PowerPC_32 : PowerPC)
    | `r64 -> (module PowerPC_64) in
  let open M.E in
  let reg = Reg_model.ec M.model in
  let load = Mem_model.load M.mem endian in
  let store = Mem_model.store M.mem endian in
  let pc = Memory.min_addr memory |>
           Exp.of_word |>
           Exp.signed in
  let jmp e = match addr_size with
    | `r32 -> RTL.(jmp (low word e))
    | `r64 -> RTL.jmp e in
  let model_findi cls i =
    match Reg_model.Exp.find M.model ~cls (`Index i) with
    | None ->
      let cls = Sexp.to_string (sexp_of_cls cls) in
      ppc_fail "%s with number %d not found" cls i
    | Some e -> e in
  let word_width = match addr_size with
    | `r32 -> word
    | `r64 -> doubleword in {
  gpr = model_findi Cls.gpr;
  fpr = model_findi Cls.fpr;
  vr  = model_findi Cls.vector;
  cr0 = model_findi cr_field 0;
  cr1 = model_findi cr_field 1;
  cr2 = model_findi cr_field 2;
  cr3 = model_findi cr_field 3;
  cr4 = model_findi cr_field 4;
  cr5 = model_findi cr_field 5;
  cr6 = model_findi cr_field 6;
  cr7 = model_findi cr_field 7;
  load; store; jmp; pc; word_width; reg;
  cr; ctr; lr; tar; so; ca; ov; ca32; ov32; }
