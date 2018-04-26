open Bap.Std
open Bap_rtl.Std

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
