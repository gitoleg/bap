open Core_kernel.Std
open Bap.Std

open Bap_rtl.Std
open Model
open Powerpc_utils
open Powerpc_model
open Powerpc_types

let make_cpu addr_size endian memory =
  let (module M) = match addr_size with
    | `r32 -> (module PowerPC_32 : PowerPC)
    | `r64 -> (module PowerPC_64) in
  let open M.E in
  let reg = Model.Reg.ec M.model in
  let load = Model.Mem.load M.mem endian in
  let store = Model.Mem.store M.mem endian in
  let pc = Memory.min_addr memory |>
           Exp.of_word |>
           Exp.signed in
  let jmp e = match addr_size with
    | `r32 -> RTL.jmp (low word e)
    | `r64 -> RTL.jmp e in
  let model_findi cls i =
    match Reg.find' M.model ~cls (`Index i) with
    | None ->
      let cls = Sexp.to_string (sexp_of_cls cls) in
      ppc_fail "%s with number %d not found" cls i
    | Some e -> e in
  let gpr = model_findi Cls.gpr in
  let fpr = model_findi Cls.fpr in
  let vr  = model_findi Cls.vector in
  let cr0 = model_findi cr_field 0 in
  let cr1 = model_findi cr_field 1 in
  let cr2 = model_findi cr_field 2 in
  let cr3 = model_findi cr_field 3 in
  let cr4 = model_findi cr_field 4 in
  let cr5 = model_findi cr_field 5 in
  let cr6 = model_findi cr_field 6 in
  let cr7 = model_findi cr_field 7 in
  let word_width = match addr_size with
    | `r32 -> word
    | `r64 -> doubleword in
  { load; store; jmp; pc; word_width;
    reg; gpr; fpr; vr;
    cr; cr0; cr1; cr2; cr3; cr4; cr5; cr6; cr7;
    ctr; lr; tar;
    so; ca; ov; ca32; ov32;}
