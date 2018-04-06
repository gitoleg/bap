open Bap.Std
open Bap_rtl_types
open Bap_rtl_bitwidth

module Exp : sig
  val of_var  : var -> exp
  val of_vars : var list -> exp
  val of_word : word -> exp
  val tmp : int -> exp

  val load : var -> exp -> endian -> size -> exp
  val extract : int -> int -> exp -> exp
  val signed : exp -> exp
  val unsigned : exp -> exp
  val concat : exp -> exp -> exp
  val cast : exp -> int -> sign -> exp
  val width : exp -> int
  val bil_exp : exp -> Bap.Std.exp
end

module Infix : sig
  val ( + )  : exp -> exp -> exp
  val ( - )  : exp -> exp -> exp
  val ( * )  : exp -> exp -> exp
  val ( / )  : exp -> exp -> exp
  val ( ^ )  : exp -> exp -> exp
  val ( % )  : exp -> exp -> exp
  val ( < )  : exp -> exp -> exp
  val ( > )  : exp -> exp -> exp
  val ( <= )  : exp -> exp -> exp
  val ( >= )  : exp -> exp -> exp
  val ( = )  : exp -> exp -> exp
  val ( <> )  : exp -> exp -> exp
  val ( << )  : exp -> exp -> exp
  val ( >> )  : exp -> exp -> exp
  val ( lor )  : exp -> exp -> exp
  val ( land ) : exp -> exp -> exp
  val ( lxor ) : exp -> exp -> exp
  val lnot : exp -> exp
end

type 'a ec

module Constructor : sig

  val imm : (op -> exp) ec
  val var : (bitwidth -> exp) ec
  val reg : (reg -> exp) -> (op -> exp) ec
  val const : (bitwidth -> int -> exp) ec
  val of_string : (string -> exp) ec
  val fixed_imm : (bitwidth -> op -> exp) ec

  val signed : 'a ec -> 'a
  val unsigned : 'a ec -> 'a

end
