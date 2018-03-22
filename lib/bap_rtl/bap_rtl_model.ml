open Core_kernel.Std
open Bap.Std

open Bap_rtl_types
open Bap_rtl_exp
open Bap_rtl_core.Rtl
open Bap_rtl_bitwidth

let size_of_width x =
  let x = int_of_bitwidth x in
  match Size.of_int x with
  | Ok s -> s
  | Error _ -> failwith (sprintf "invalid size: %d" x)

module Mem = struct

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
end

module Reg = struct

  type 'a t = 'a String.Map.t * 'a Int.Map.t

  type alias = [
    | `Index of int
    | `Name of string
  ]

  let empty = String.Map.empty, Int.Map.empty

  let add_aliases model aliases data =
    List.fold aliases ~init:model
      ~f:(fun (names, indxs) -> function
          | `Index i -> names, Int.Map.add indxs i data
          | `Name n -> String.Map.add names n data, indxs)

  let add (names, indxs) ?(aliases=[]) name data =
    let names = Map.add names name data in
    add_aliases (names,indxs) aliases data

  let find (names,_) name = Map.find_exn names name
  let find' model reg = find model (Reg.name reg)
  let findi (_,inds) ind = Map.find_exn inds ind

  module Var = struct

    let make_reg name width = Var.create name (Type.Imm width)

    let define_full model ?(aliases=[]) name width =
      let var = make_reg name width in
      add model ~aliases name var, var

    let add model ?(aliases=[]) name width =
      let model, _ = define_full model ~aliases name width in
      model

    let add' = define_full
  end

  module Exp = struct

    let of_var (names,indxs) : exp t =
      let to_exp m init =
        List.fold (Map.to_alist m) ~init
          ~f:(fun m (key, v) -> Map.add m key (Exp.of_var v)) in
      to_exp names String.Map.empty,
      to_exp indxs Int.Map.empty

  end

end
