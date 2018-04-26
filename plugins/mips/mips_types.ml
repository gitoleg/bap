open Core_kernel.Std
open Bap.Std
open Bap_rtl.Std
open Mips_utils
open Mips_model

type cpu = {
  load : 'a. 'a exp -> bitwidth -> rhs exp;
  store : 'a 'b. 'a exp -> 'b exp -> bitwidth -> rtl;
  jmp : 'a. 'a exp -> rtl;
  cia : rhs exp;
  word_width : rhs exp;
  word_bitwidth : bitwidth;
  reg : (op -> lhs exp) ec;
  gpr : int -> lhs exp;
  fpr : int -> lhs exp;
  hi : lhs exp;
  lo : lhs exp;
}
