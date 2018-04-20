open Core_kernel.Std
open Bap.Std
open Bap_rtl.Std

module type Model = sig
  type t
  val hi : t
  val lo : t
end

module type MIPS = sig
  module E : Model with type t := exp
  include Model with type t := var
  val model : reg_model
  val mem : var
  val gpr_bitwidth : int
  val fpr_bitwidth : int
end

let range32 = List.range 0 32

let make_reg width name = Var.create name (Type.imm width)

module Bitwidth = struct
  let fpr_bitwidth = 64
  let cr_bitwidth = 32
  let lr_bitwidth = 32
end

module type Spec = sig
  val gpr_bitwidth : int
  val addr_size : addr_size
end

module Make_MIPS(S: Spec) : MIPS = struct
  include Bitwidth
  let gpr_bitwidth = S.gpr_bitwidth

  let make_gpr = make_reg gpr_bitwidth

  let model = Reg_model.empty

  let gprs = [
    make_gpr "ZERO", "R0";
    make_gpr "AT", "R1";
    make_gpr "V0", "R2";
    make_gpr "V1", "R3";
    make_gpr "A0", "R4";
    make_gpr "A1", "R5";
    make_gpr "A2", "R6";
    make_gpr "A3", "R7";
    make_gpr "T0", "R8";
    make_gpr "T1", "R9";
    make_gpr "T2", "R10";
    make_gpr "T3", "R11";
    make_gpr "T4", "R12";
    make_gpr "T5", "R13";
    make_gpr "T6", "R14";
    make_gpr "T7", "R15";
    make_gpr "S0", "R16";
    make_gpr "S1", "R17";
    make_gpr "S2", "R18";
    make_gpr "S3", "R19";
    make_gpr "S4", "R20";
    make_gpr "S5", "R21";
    make_gpr "S6", "R22";
    make_gpr "S7", "R23";
    make_gpr "T8", "R24";
    make_gpr "T9", "R25";
    make_gpr "K0", "R26";
    make_gpr "K1", "R27";
    make_gpr "GP", "R28";
    make_gpr "SP", "R29";
    make_gpr "FP", "R30";
    make_gpr "RA", "R31";
  ]

  let model = List.foldi gprs ~init:model ~f:(fun ind model (reg,alias) ->
      let aliases = [`Name alias; `Index ind] in
      let aliases = match S.addr_size with
        | `r32 -> aliases
        | `r64 ->
          `Name (Var.name reg ^ "_64") :: aliases in
      Reg_model.add ~aliases Cls.gpr reg model)

  let model = List.fold range32 ~init:model ~f:(fun model i ->
      let reg = make_reg fpr_bitwidth (sprintf "f%d" i) in
      Reg_model.add ~aliases:[`Index i] Cls.fpr reg model)

  let hi = make_reg gpr_bitwidth "HI"
  let lo = make_reg gpr_bitwidth "LO"

  let model = List.fold [hi; lo] ~init:model
      ~f:(fun m x -> Reg_model.add Cls.system x m)

  module E = struct
    let hi = Exp.of_var hi
    let lo = Exp.of_var lo
  end

  let mem = Var.create "mem" (Type.mem S.addr_size `r8)
end

module Spec32 = struct
  let gpr_bitwidth = 32
  let addr_size = `r32
end

module Spec64 = struct
  let gpr_bitwidth = 64
  let addr_size = `r64
end

module MIPS_32 = Make_MIPS(Spec32)
module MIPS_64 = Make_MIPS(Spec64)

module Make_cpu(M : MIPS) : CPU = struct
  open M

  let mem = M.mem

  let gpr = Reg_model.all model Cls.gpr |> Var.Set.of_list

  let sp = Var.Set.find_exn gpr ~f:(fun v -> String.is_prefix ~prefix:"SP" (Var.name v))
  let fp = Var.Set.find_exn gpr ~f:(fun v -> String.is_prefix ~prefix:"FP" (Var.name v))

  (* MIPS doesn't have flags, but structure requires them
   * We just make a stubs here *)
  let flag n = Var.create n bool_t
  let zf = flag "ZF"
  let cf = flag "CF"
  let vf = flag "VF"
  let nf = flag "NF"
  let flags = Var.Set.of_list [ vf; cf; nf; zf; ]
  let is = Var.same
  let is_reg r = Set.mem gpr (Var.base r)
  let is_mem = is mem
  let is_sp = is sp
  let is_bp = is fp
  let is_flag _ = false
  let is_zf _ = false
  let is_cf _ = false
  let is_vf _ = false
  let is_nf _ = false
end

module MIPS_32_cpu = Make_cpu(MIPS_32)
module MIPS_64_cpu = Make_cpu(MIPS_64)
