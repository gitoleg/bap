open Core_kernel.Std
open Bap.Std
open Bap_rtl_types
open Bap_rtl_bitwidth

val zero : exp
val one  : exp
val ones : exp

(** [low width e] - extracts low [width] bits from [e]  *)
val low : bitwidth -> exp -> exp

(** [high width e] - extracts high [width] bits from [e]  *)
val high : bitwidth -> exp -> exp

(** [first e n] - extracts first [n] bits from [e], starting from
    the most significant bit *)
val first : exp -> int -> exp

(** [last e n] - extracts last [n] bits from [e], where the
    last bit is the least significant bit *)
val last : exp -> int -> exp

(** [nth width e n] - extracts a portion of [e] of width [width] at
    index [n], where each index points to a portion of width [width].
    Indexes are zero based and started from most significant portion.
    E.g. [nth halfword e 1] extracts a second halfword from [e] *)
val nth : bitwidth -> exp -> int -> exp

(** [msb e] - extracts the most significant bit from [e] *)
val msb : exp -> exp

(** [lsb e] - extracts the least significant bit from [e] *)
val lsb : exp -> exp

(** [norm_jumps bil] - evaluates as best as it possible all jumps
    destinations in bil *)
val norm_jumps : bil -> bil
