open Core_kernel.Std
open Bap.Std

open Bap_rtl_exp
open Bap_rtl_core.Rtl
open Bap_rtl_bitwidth

let size_of_width x =
  let x = int_of_bitwidth x in
  match Size.of_int x with
  | Ok s -> s
  | Error _ -> failwith (sprintf "invalid size: %d" x)

module type M = sig
  val mem : var
  val endian : endian
end

module Make(M : M) = struct
  open M

  let load addr width =
    let size = size_of_width width in
    Exp.load mem addr endian size

  let store addr data width =
    let size = size_of_width width in
    store mem addr data endian size
end
