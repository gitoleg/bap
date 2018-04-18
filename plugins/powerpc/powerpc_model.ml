open Core_kernel.Std
open Bap.Std

open Bap_rtl.Std
open Model

module type Model = sig
  type t
  val ctr : t
  val lr  : t
  val tar : t
  val so : t
  val ca : t
  val ov : t
  val ca32 : t
  val ov32 : t
end

module type Bitwidth = sig
  val gpr_bitwidth : int
  val fpr_bitwidth : int
  val lr_bitwidth  : int
  val ctr_bitwidth : int
  val tar_bitwidth : int
  val cr_bitwidth  : int
  val vr_bitwidth  : int
end

module type PowerPC = sig
  val model : reg_model
  val mem : var
  include Bitwidth
  include Model with type t := var

  module E  : sig
    include Model with type t := exp
    val cr  : exp
  end
end

let cr_bit = Cls.of_string "CRbit"
let cr_field = Cls.of_string "CRfield"

module Vars (B : Bitwidth) = struct
  open B

  let range32 = List.range 0 32
  let model = Reg.create ()

  let reg n w = Var.create n (Type.imm w)
  let bit n = Var.create n (Type.imm 1)

  (** fixed precision flags  *)
  let so = bit "SO"
  let ca = bit "CA"
  let ov = bit "OV"
  let ca32 = bit "CA32"
  let ov32 = bit "OV32"

  (** FPRF floating point result flags  *)
  let fc = bit "C"  (** Result Class Descriptor         *)
  let fl = bit "FL" (** Less Than or Negative           *)
  let fe = bit "FE" (** Greater Than or Positive        *)
  let fg = bit "FG" (** Floating-Point Equal or Zero    *)
  let fu = bit "FU" (** Floating-Point Unordered or NaN *)

  (** system registers  *)
  let ctr = reg "CTR" ctr_bitwidth
  let lr  = reg "LR"  lr_bitwidth
  let tar = reg "TAR" tar_bitwidth

  let gprs = List.map range32 ~f:(fun i ->
      Cls.gpr,
      reg (sprintf "R%d" i) gpr_bitwidth,
      [`Name (sprintf "X%d" i); `Index i])

  let fprs = List.map range32 ~f:(fun i ->
      Cls.fpr, reg (sprintf "F%d" i) fpr_bitwidth, [`Index i])

  let vr = List.map range32 ~f:(fun i ->
      Cls.vector, reg (sprintf "V%d" i)vr_bitwidth, [`Index i])

  let regs = List.concat [gprs; fprs; vr;]

  let () = List.iter regs
      ~f:(fun (cls, reg, aliases) -> Reg.add model cls ~aliases reg)

  let () = List.iter ~f:(Reg.add model Cls.flag)
      [so; ca; ov; ca32; ov32; fc; fl; fe; fg; fu]

  let cr_fields = [
    "CR0", 0, ("CR0UN", "CR0EQ", "CR0GT", "CR0LT");
    "CR1", 1, ("CR1UN", "CR1EQ", "CR1GT", "CR1LT");
    "CR2", 2, ("CR2UN", "CR2EQ", "CR2GT", "CR2LT");
    "CR3", 3, ("CR3UN", "CR3EQ", "CR3GT", "CR3LT");
    "CR4", 4, ("CR4UN", "CR4EQ", "CR4GT", "CR4LT");
    "CR5", 5, ("CR5UN", "CR5EQ", "CR5GT", "CR5LT");
    "CR6", 6, ("CR6UN", "CR6EQ", "CR6GT", "CR6LT");
    "CR7", 7, ("CR7UN", "CR7EQ", "CR7GT", "CR7LT");
  ]

  (** check alias indexes here  *)
  let () =
    List.iter cr_fields
      ~f:(fun (field, ind, (bit0,bit1,bit2,bit3)) ->
        let bit0 = bit bit0 in
        let bit1 = bit bit1 in
        let bit2 = bit bit2 in
        let bit3 = bit bit3 in
        let bit_index = 31 - ind * 4 in (** reverse indexes  *)
        Reg.add model cr_bit ~aliases:[`Index (bit_index - 0)] bit0;
        Reg.add model cr_bit ~aliases:[`Index (bit_index - 1)] bit1;
        Reg.add model cr_bit ~aliases:[`Index (bit_index - 2)] bit2;
        Reg.add model cr_bit ~aliases:[`Index (bit_index - 3)] bit3;
        let e = Exp.of_vars [bit3;bit2;bit1;bit0] in
        Reg.add' model cr_field ~aliases:[`Index ind] (`Name field) e)

end

module type Spec = sig
  val gpr_bitwidth : int
  val addr_size : addr_size
end

module Make_ppc(S : Spec) : PowerPC = struct

  module Bitwidth = struct
    let gpr_bitwidth = S.gpr_bitwidth
    let fpr_bitwidth = S.gpr_bitwidth
    let lr_bitwidth  = S.gpr_bitwidth
    let ctr_bitwidth = S.gpr_bitwidth
    let tar_bitwidth = S.gpr_bitwidth
    let cr_bitwidth  = 32
    let vr_bitwidth  = 128
  end

  let mem = Var.create "mem" (Type.mem S.addr_size `r8)

  module Vars = Vars(Bitwidth)
  module E = struct
    type t = exp

    open Vars

    let so = Exp.of_var so
    let ca = Exp.of_var ca
    let ov = Exp.of_var ov
    let ca32 = Exp.of_var ca32
    let ov32 = Exp.of_var ov32

    let ctr = Exp.of_var ctr
    let lr  = Exp.of_var lr
    let tar = Exp.of_var tar

    let cr =
      List.map range32
        ~f:(fun i -> Reg.find_exn model ~cls:cr_bit (`Index i)) |>
      Exp.of_vars
  end

  include Vars
  include Bitwidth

end

module Spec32 = struct
  let gpr_bitwidth = 32
  let addr_size = `r32
end

module Spec64 = struct
  let gpr_bitwidth = 64
  let addr_size = `r64
end

module PowerPC_32 = Make_ppc(Spec32)
module PowerPC_64 = Make_ppc(Spec64)

module Make_cpu(P : PowerPC) : CPU = struct
  open P

  let mem = P.mem
  let gpr = Reg.all P.model Cls.gpr |> Var.Set.of_list
  let sp = Var.Set.find_exn gpr ~f:(fun v -> Var.name v = "R1")
  let vf = ov
  let cf = ca
  let nf = Reg.find_exn model ~cls:cr_bit (`Index 0)
  let zf = Reg.find_exn model ~cls:cr_bit (`Index 1)
  let flags = Var.Set.of_list @@ Reg.all model Cls.flag
  let is = Var.same
  let is_reg r = Set.mem gpr (Var.base r)
  let is_flag r = Set.mem flags (Var.base r)
  let is_zf = is zf
  let is_cf = is ca
  let is_vf = is vf
  let is_nf = is nf
  let is_mem = is mem
  let is_sp = is sp
  let is_bp _ = false
end

module PowerPC_32_cpu = Make_cpu(PowerPC_32)
module PowerPC_64_cpu = Make_cpu(PowerPC_64)
