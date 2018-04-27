open Core_kernel.Std
open Bap_rtl_types

val bit  : bitwidth
val byte : bitwidth
val halfword : bitwidth
val word : bitwidth
val doubleword : bitwidth
val quadword : bitwidth
val bitwidth_of_int : int -> bitwidth
val int_of_bitwidth : bitwidth -> int
