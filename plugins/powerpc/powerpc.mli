open Core_kernel.Std
open Bap.Std
open Bap_rtl.Std

(** Powerpc standart library

    Contains some definitions from Bap_rtl to achieve
    a weird PowerPC bits numering and other stuff
    like PowerPC models. So the main idea is still the
    same:
    [ open Powerpc.Std ]
    in the very beginnig of any file with liftted
    instructions *)

module Std : sig

  module RTL : module type of RTL
  include module type of Bitwidth
  include module type of Ec
  module Array : module type of Array

  type cpu = {
    load       : 'a. 'a exp -> bitwidth -> rhs exp;
    store      : 'a 'b. 'a exp -> 'b exp -> bitwidth -> rtl;
    jmp        : 'a. 'a exp -> rtl;
    pc         : rhs exp;
    word_width : bitwidth;

    (** registers  *)
    reg       : (op -> lhs exp) ec; (** construct exp from register *)
    gpr       : int -> lhs exp; (** general purpose registers 0..31 *)
    fpr       : int -> lhs exp; (** floating-point registers 0..31  *)
    vr        : int -> lhs exp; (** vector register 0..31           *)
    ctr       : lhs exp;       (** count register      *)
    lr        : lhs exp;       (** link register       *)
    tar       : lhs exp;       (** target register     *)
    cr        : lhs exp;       (** condition register  *)
    cr0       : lhs exp;       (** condition register field 0 *)
    cr1       : lhs exp;       (** condition register field 1 *)
    cr2       : lhs exp;       (** condition register field 2 *)
    cr3       : lhs exp;       (** condition register field 3 *)
    cr4       : lhs exp;       (** condition register field 4 *)
    cr5       : lhs exp;       (** condition register field 5 *)
    cr6       : lhs exp;       (** condition register field 6 *)
    cr7       : lhs exp;       (** condition register field 7 *)

    (** fixed precision flags *)
    so        : lhs exp; (** summary overflow        *)
    ca        : lhs exp; (** carry flag              *)
    ov        : lhs exp; (** overflow flag           *)
    ca32      : lhs exp; (** carry out of 32 bits    *)
    ov32      : lhs exp; (** overflow of 32 bits     *)
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

  (** [width e] returns a width of [e] as an expression *)
  val width : 'a exp -> rhs exp

  (** class of condition register bit   *)
  val cr_bit   : cls

  (** class of condition register field *)
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

  type lexp = lhs exp

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
      include Model with type t := lexp

      (** condition register  *)
      val cr  : lhs exp
    end
  end

  module PowerPC_32 : PowerPC
  module PowerPC_64 : PowerPC

  module T32 : Target
  module T64 : Target
  module T64_le : Target

end
