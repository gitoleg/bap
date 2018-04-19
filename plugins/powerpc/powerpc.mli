open Core_kernel.Std
open Bap.Std
open Bap_rtl.Std

(** Powerpc standart library

    Contains some definitions from Bap_rtl to achieve
    a weird PowerPc arch bits numering and other stuff
    like PowerPC models. So the main idea is still the
    same: all one needs to complement the lifter is just
    [ open Powerpc.Std ]
*)

module Std : sig

  module RTL : module type of RTL
  include module type of Bitwidth
  include module type of Ec

  type cpu = {
    load       : exp -> bitwidth -> exp;
    store      : exp -> exp -> bitwidth -> rtl;
    jmp        : exp -> rtl;
    pc         : exp;
    word_width : bitwidth;

    (** registers  *)
    reg       : (op -> exp) ec; (** construct exp from register *)
    gpr       : int -> exp; (** general purpose registers 0..31 *)
    fpr       : int -> exp; (** floating-point registers 0..31  *)
    vr        : int -> exp; (** vector register 0..31           *)
    ctr       : exp;       (** count register      *)
    lr        : exp;       (** link register       *)
    tar       : exp;       (** target register     *)
    cr        : exp;       (** condition register  *)
    cr0       : exp;       (** condition register field 0 *)
    cr1       : exp;       (** condition register field 1 *)
    cr2       : exp;       (** condition register field 2 *)
    cr3       : exp;       (** condition register field 3 *)
    cr4       : exp;       (** condition register field 4 *)
    cr5       : exp;       (** condition register field 5 *)
    cr6       : exp;       (** condition register field 6 *)
    cr7       : exp;       (** condition register field 7 *)

    (** fixed precision flags *)
    so        : exp; (** summary overflow        *)
    ca        : exp; (** carry flag              *)
    ov        : exp; (** overflow flag           *)
    ca32      : exp; (** carry out of 32 bits    *)
    ov32      : exp; (** overflow of 32 bits     *)
  }


  type lift = cpu -> op array -> rtl list

  (** [concat insn insn'] - returns a lifter, that is a
      concatenation of code for insn and insn' *)
  val concat : lift -> lift -> lift

  (** [^] same as concat  *)
  val (^) : lift -> lift -> lift

  (** Registration *)

  (** [name >| lift]  - registers a lifter for instruction [name]  *)
  val (>|) : string -> lift -> unit

  (** [name >. lift] - registers a lifter for dot version of instruction
      [name], but also extend an RTL code with signed comparison of the
      result to a zero and writing CR0 field according to this
      comparison. It's also assumed that a first instruction operand
      is used for storing of a result. *)
  val (>.) : string -> lift -> unit

  val width : exp -> exp


  val cr_bit   : cls
  val cr_field : cls


  module type Model = sig
    type t

    (** count register  *)
    val ctr : t

    (** link register  *)
    val lr : t

    (** target register  *)
    val tar : t

    (** fixed precision flags *)
    val so : t   (** summary overflow        *)
    val ca : t   (** carry flag              *)
    val ov : t   (** overflow flag           *)
    val ca32 : t (** carry out of 32 bits    *)
    val ov32 : t (** overflow of 32 bits     *)
  end

  module type PowerPC = sig
    val model : reg_model
    val mem : var
    val gpr_bitwidth : int
    val fpr_bitwidth : int
    val lr_bitwidth  : int
    val ctr_bitwidth : int
    val tar_bitwidth : int
    val cr_bitwidth  : int
    val vr_bitwidth  : int

    include Model with type t := var

    module E  : sig
      include Model with type t := exp

      (** condition register  *)
      val cr  : exp
    end
  end

  module PowerPC_32 : PowerPC
  module PowerPC_64 : PowerPC

  module PowerPC_32_cpu : CPU
  module PowerPC_64_cpu : CPU

  module T32 : Target
  module T64 : Target
  module T64_le : Target

end
