open Core_kernel.Std
open Bap.Std
open Bap_rtl_types
open Bap_rtl_bitwidth

val zero : rhs exp
val one  : rhs exp
val ones : rhs exp

(** [low width e] - extracts low [width] bits from [e]  *)
val low : bitwidth -> 'a exp -> 'a exp

(** [high width e] - extracts high [width] bits from [e]  *)
val high : bitwidth -> 'a exp -> 'a exp

(** [first e n] - extracts first [n] bits from [e], starting from
    the most significant bit *)
val first : 'a exp -> int -> 'a exp

(** [last e n] - extracts last [n] bits from [e], where the
    last bit is the least significant bit *)
val last : 'a exp -> int -> 'a exp

(** [nth width e n] - extracts a portion of [e] of width [width] at
    index [n], where each index points to a portion of width [width].
    Indexes are zero based and started from most significant portion.
    E.g. [nth halfword e 1] extracts a second halfword from [e] *)
val nth : bitwidth -> 'a exp -> int -> 'a exp

(** [msb e] - extracts the most significant bit from [e] *)
val msb : 'a exp -> 'a exp

(** [lsb e] - extracts the least significant bit from [e] *)
val lsb : 'a exp -> 'a exp
