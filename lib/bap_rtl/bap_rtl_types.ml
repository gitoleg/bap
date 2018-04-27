open Core_kernel.Std
open Bap.Std

type sign = Signed | Unsigned [@@deriving bin_io, compare, sexp]

type binop = Bil.binop [@@deriving bin_io, compare, sexp]
type unop  = Bil.unop  [@@deriving bin_io, compare, sexp]

type body =
  | Vars of var * var list
  | Word of word
  | Load of (var * body * endian * size)
  | Concat of body * body
  | Binop of binop * body * body
  | Extract of (int * int * body)
  | Cast of (sign * int * body)
  | Unop of (unop * body)
  | Pattern of word
[@@deriving bin_io, compare, sexp]

type uexp = {
  body : body;
  sign : sign;
  width : int;
} [@@deriving bin_io, compare, sexp]

type lhs [@@deriving bin_io, compare, sexp]
type rhs [@@deriving bin_io, compare, sexp]

type 'a exp = uexp [@@deriving bin_io, compare, sexp]

type rtl =
  | Move of uexp * uexp
  | Jmp of uexp
  | Store of var * uexp * uexp * endian * size
  | If of uexp * rtl list * rtl list
  | Block of rtl list
  | Message of string
[@@deriving bin_io, compare, sexp]


type bitwidth = int [@@deriving bin_io, compare, sexp]
