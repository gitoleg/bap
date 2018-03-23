open Core_kernel.Std
open Bap.Std

(* open Powerpc_rtl *)
open Bap_rtl.Std

module type Model = sig
  type t
  val gpr : t reg_model
  val fpr : t reg_model
  val vr  : t reg_model
  val ctr : t
  val lr  : t
  val tar : t
  val cr : t reg_model
  val so : t
  val ca : t
  val ov : t
  val ca32 : t
  val ov32 : t
end

module type Model_exp = sig
  include Model with type t := exp
  (** condition register  *)
  val cr : exp

  (** condition register fields *)
  val cr_fields  : exp reg_model

end

module type PowerPC = sig
  module E : Model_exp
  include Model with type t := var

  val mem : var
  val flags : Var.Set.t
  val gpr_bitwidth : int
  val fpr_bitwidth : int
  val vr_bitwidth  : int
  val cr_bitwidth  : int
  val lr_bitwidth  : int
  val ctr_bitwidth : int
  val tar_bitwidth : int
end

let range32 = List.range 0 32
let range64 = List.range 0 64

let flag name = Var.create name (Type.imm 1)

module Bitwidth = struct
  let fpr_bitwidth = 64
  let vr_bitwidth  = 128
  let cr_bitwidth  = 32
  let lr_bitwidth  = 64
  let ctr_bitwidth = 64
  let tar_bitwidth = 64
end

module Vars = struct
  open Bitwidth

  let make () = Model.Reg.create ()
  let add = Model.Reg.add_reg

  let gpr = make ()
  let fpr = make ()
  let vr  = make ()

  let () = List.iter range32 (fun i ->
      let gname = sprintf "R%d" i in
      let xname = sprintf "X%d" i in
      let fname = sprintf "F%d" i in
      let vname = sprintf "VR%d" i in
      add gpr ~aliases:[`Name xname; `Index i] gname fpr_bitwidth;
      add fpr ~aliases:[`Index i] fname fpr_bitwidth;
      add vr  ~aliases:[`Index i] vname vr_bitwidth;)

  (** count register  *)
  let ctr = Var.create "CTR" (Type.imm ctr_bitwidth)

  (** link register  *)
  let lr = Var.create "LR" (Type.imm lr_bitwidth)

  (** target register  *)
  let tar = Var.create "TAR" (Type.imm tar_bitwidth)

  (** fixed precision flags  *)
  let so = flag "SO" (** summary overflow *)
  let ca = flag "CA"
  let ov = flag "OV"
  let ca32 = flag "CA32" (** carry of low-order 32 bit result *)
  let ov32 = flag "OV32" (** overflow of low-order 32 bit result *)

  (** FPRF floating point result flags  *)
  let float_c = flag "C"          (** Result Class Descriptor        *)
  let float_less = flag "FL"      (** Less Than or Negative           *)
  let float_equal = flag "FE"     (** Greater Than or Positive        *)
  let float_greater = flag "FG"   (** Floating-Point Equal or Zero    *)
  let float_unordered = flag "FU" (** Floating-Point Unordered or NaN *)

  (** condition register *)
  let cr = make ()

  (** condition register bit *)
  let cr_bit ind name =
    Model.Reg.add_reg' cr ~aliases:[`Index ind] name 1

  (** condition register bits  *)
  let cr0  = cr_bit 31 "CR7UN"
  let cr1  = cr_bit 30 "CR7EQ"
  let cr2  = cr_bit 29 "CR7GT"
  let cr3  = cr_bit 28 "CR7LT"
  let cr4  = cr_bit 27 "CR6UN"
  let cr5  = cr_bit 26 "CR6EQ"
  let cr6  = cr_bit 25 "CR6GT"
  let cr7  = cr_bit 24 "CR6LT"
  let cr8  = cr_bit 23 "CR5UN"
  let cr9  = cr_bit 22 "CR5EQ"
  let cr10 = cr_bit 21 "CR5GT"
  let cr11 = cr_bit 20 "CR5LT"
  let cr12 = cr_bit 19 "CR4UN"
  let cr13 = cr_bit 18 "CR4EQ"
  let cr14 = cr_bit 17 "CR4GT"
  let cr15 = cr_bit 16 "CR4LT"
  let cr16 = cr_bit 15 "CR3UN"
  let cr17 = cr_bit 14 "CR3EQ"
  let cr18 = cr_bit 13 "CR3GT"
  let cr19 = cr_bit 12 "CR3LT"
  let cr20 = cr_bit 11 "CR2UN"
  let cr21 = cr_bit 10 "CR2EQ"
  let cr22 = cr_bit 9 "CR2GT"
  let cr23 = cr_bit 8 "CR2LT"
  let cr24 = cr_bit 7 "CR1UN"
  let cr25 = cr_bit 6 "CR1EQ"
  let cr26 = cr_bit 5 "CR1GT"
  let cr27 = cr_bit 4 "CR1LT"
  let cr28 = cr_bit 3 "CR0UN"
  let cr29 = cr_bit 2 "CR0EQ"
  let cr30 = cr_bit 1 "CR0GT"
  let cr31 = cr_bit 0 "CR0LT"

  let cr_fields = [
    "CR0", 0, (cr28, cr29, cr30, cr31);
    "CR1", 1, (cr24, cr25, cr26, cr27);
    "CR2", 2, (cr20, cr21, cr22, cr23);
    "CR3", 3, (cr16, cr17, cr18, cr19);
    "CR4", 4, (cr12, cr13, cr14, cr15);
    "CR5", 5, (cr8,  cr9,  cr10, cr11);
    "CR6", 6, (cr4,  cr5,  cr6,  cr7);
    "CR7", 7, (cr0,  cr1,  cr2,  cr3);
  ]

end

module Exps = struct
  open Vars

  let to_exp = Model.Reg.exp_of_var

  let fpr = to_exp fpr
  let vr = to_exp vr
  let ctr = Exp.of_var ctr
  let lr  = Exp.of_var lr
  let tar = Exp.of_var tar
  let so  = Exp.of_var so
  let ca  = Exp.of_var ca
  let ov  = Exp.of_var ov
  let ca32 = Exp.of_var ca32
  let ov32 = Exp.of_var ov32

  let cr = to_exp cr

  let cr_fields = Model.Reg.create ()

  let cr_fields =
    List.iter Vars.cr_fields
      ~f:(fun (name,ind,(b3,b2,b1,b0)) ->
          let e = Exp.of_vars [b0;b1;b2;b3] in
          Model.Reg.add cr_fields ~aliases:[`Index ind] name e)
end

module type Spec = sig
  val gpr_bitwidth : int
  val addr_size : addr_size
end

module Make_ppc(S : Spec) : PowerPC = struct
  include Bitwidth
  let gpr_bitwidth = S.gpr_bitwidth

  include Vars

  module E = struct
    include Exps

  end

  let mem = Var.create "mem" (Type.mem S.addr_size `r8)

  let flags = Var.Set.of_list [
      so; ca; ca32; ov; ov32;
      Model.Reg.Exn.findi cr 0;
      Model.Reg.Exn.findi cr 1;
      Model.Reg.Exn.findi cr 2;
    ]

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

  let gpr =
    let data = Map.data gpr in
    List.fold data ~init:Var.Set.empty
      ~f:(fun regs v -> Var.Set.add regs v)

  let sp = Var.Set.find_exn gpr ~f:(fun v -> Var.name v = "R1")
  let vf = ov
  let cf = ca
  let nf = Map.find_exn cri 0
  let zf = Map.find_exn cri 1

  let flags = Var.Set.of_list [
      so; ca; ov; cf; nf; zf; ca32; ov32;
    ]

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
