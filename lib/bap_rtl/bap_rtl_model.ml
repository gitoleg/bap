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

  type 'a t = 'a String.Table.t * 'a Int.Table.t

  type alias = [
    | `Index of int
    | `Name of string
  ]

  let create () = String.Table.create (), Int.Table.create ()

  let add_aliases (names,indxs) aliases data =
    List.iter aliases
      ~f:(function
          | `Index i -> Hashtbl.add_exn indxs i data
          | `Name n -> Hashtbl.add_exn names n data)

  let add (names, indxs) ?(aliases=[]) name data =
    Hashtbl.add_exn names name data;
    add_aliases (names,indxs) aliases data

  let find (names,_) name = Hashtbl.find_exn names name
  let find' model reg = find model (Reg.name reg)
  let findi (_,inds) ind = Hashtbl.find_exn inds ind

  module Var = struct

    let make_reg name width = Var.create name (Type.Imm width)

    let define_full model ?(aliases=[]) name width =
      let var = make_reg name width in
      add model ~aliases name var;
      var

    let add model ?(aliases=[]) name width =
      ignore @@ define_full model ~aliases name width

    let add' = define_full
  end

  module Exp = struct

    let of_var (names,indxs) : exp t =
      let to_exp m init =
        List.iter (Hashtbl.to_alist m)
          ~f:(fun (key, v) -> Hashtbl.add_exn init key (Exp.of_var v)) in
      let (names', indxs') as model = create () in
      to_exp names names';
      to_exp indxs indxs';
      model
  end

end
