open Bap.Std
open Bap_rtl_types
open Bap_rtl_bitwidth

module Exp : sig
  val of_var  : var -> lhs exp
  val of_vars : var list -> lhs exp
  val tmp : int -> lhs exp
  val of_word : word -> rhs exp
  val pattern : word -> rhs exp

  val load : var -> 'a exp -> endian -> size -> rhs exp
  val extract : int -> int -> 'a exp -> 'a exp
  val signed : 'a exp -> 'a exp
  val unsigned : 'a exp -> 'a exp
  val concat : 'a exp -> 'b exp -> rhs exp
  val cast : 'a exp -> int -> sign -> 'a exp
  val width : 'a exp -> int
  val bil_exp : 'a exp -> Bap.Std.exp
  val ignore : 'a exp -> uexp
end

module Infix : sig
  val ( + )  : 'a exp -> 'b exp -> rhs exp
  val ( - )  : 'a exp -> 'b exp -> rhs exp
  val ( * )  : 'a exp -> 'b exp -> rhs exp
  val ( / )  : 'a exp -> 'b exp -> rhs exp
  val ( ^ )  : 'a exp -> 'b exp -> rhs exp
  val ( % )  : 'a exp -> 'b exp -> rhs exp
  val ( < )  : 'a exp -> 'b exp -> rhs exp
  val ( > )  : 'a exp -> 'b exp -> rhs exp
  val ( <= )  : 'a exp -> 'b exp -> rhs exp
  val ( >= )  : 'a exp -> 'b exp -> rhs exp
  val ( = )  : 'a exp -> 'b exp -> rhs exp
  val ( <> )  : 'a exp -> 'b exp -> rhs exp
  val ( << )  : 'a exp -> 'b exp -> rhs exp
  val ( >> )  : 'a exp -> 'b exp -> rhs exp
  val ( lor )  : 'a exp -> 'b exp -> rhs exp
  val ( land ) : 'a exp -> 'b exp -> rhs exp
  val ( lxor ) : 'a exp -> 'b exp -> rhs exp
  val lnot : 'a exp -> rhs exp
end

type 'a ec

module Constructor : sig

  val imm : (op -> rhs exp) ec
  val var : (bitwidth -> lhs exp) ec
  val reg : (reg -> lhs exp) -> (op -> lhs exp) ec
  val const : (bitwidth -> int -> rhs exp) ec
  val of_string : (string -> rhs exp) ec
  val fixed_imm : (bitwidth -> op -> rhs exp) ec

  val signed : 'a ec -> 'a
  val unsigned : 'a ec -> 'a

end
