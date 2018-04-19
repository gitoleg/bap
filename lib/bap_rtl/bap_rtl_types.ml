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

type exp = {
  body : body;
  sign : sign;
  width : int;
} [@@deriving bin_io, compare, sexp]

type rtl =
  | Move of exp * exp
  | Jmp of exp
  | Store of var * exp * exp * endian * size
  | If of exp * rtl list * rtl list
  | Foreach of bool * exp * exp * rtl list
  | Message of string
[@@deriving bin_io, compare, sexp]


type bitwidth = int [@@deriving bin_io, compare, sexp]
