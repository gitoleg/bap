open Core_kernel.Std
open Bap.Std
open Regular.Std

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

  module Name = struct

    type t = [
      | `Index of int
      | `Name of string
    ] [@@deriving bin_io,compare,sexp]

    include Regular.Make(struct
        type nonrec t = t [@@deriving bin_io,compare,sexp]

        let module_name = Some "Model.Reg.Name"
        let hash = Hashtbl.hash
        let version = "0.1"
        let pp fmt = function
          | `Index i -> Format.fprintf fmt "index %d" i
          | `Name n -> Format.fprintf fmt "name %s" n
      end)
  end

  type name = Name.t
  type data = var option * exp
  type t = data Name.Table.t

  let create () = Name.Table.create ()
  let update t key data = Hashtbl.update t key (fun _ -> data)

  let add_reg t ?(aliases=[]) var =
    let names = `Name (Var.name var) :: aliases in
    let data = Some var, Exp.of_var var in
    List.iter names
      ~f:(fun name -> Hashtbl.update t name (fun _ -> data))

  let add_exp t ?(aliases=[]) name exp =
    List.iter (name :: aliases)
      ~f:(fun name ->
          Hashtbl.update t name (function
              | None -> None, exp
              | Some (v, _) -> v, exp))

  let find t n = Hashtbl.find t (`Name n)
  let find_ind t i = Hashtbl.find t (`Index i)

  let get_reg = function
    | Some (v, _) -> v
    | _ -> None

  let get_exp = function
    | Some (_, e) -> Some e
    | _ -> None

  let reg  t n = find t n |> get_reg
  let regi t i = find_ind t i |> get_reg
  let exp  t n = find t n |> get_exp
  let expi t i = find_ind t i |> get_exp

  module Exn = struct

    let er_msg what = function
      | `Index i -> sprintf "%s not found by index %d" what i
      | `Name n -> sprintf "%s not found by name %s" what n

    let get_reg x = function
      | Some (Some r, _) -> r
      | _ -> failwith (er_msg "register" x)

    let get_exp x = function
      | Some (_, e) -> e
      | _ -> failwith (er_msg "expression" x)

    let find_reg t x = Hashtbl.find t x |> get_reg x
    let find_exp t x = Hashtbl.find t x |> get_exp x

    let reg  t n = find_reg t (`Name n)
    let regi t i = find_reg t (`Index i)
    let exp  t n = find_exp t (`Name n)
    let expi t i = find_exp t (`Index i)
  end


  let reg_ec t =
    let find reg = Exn.exp t (Reg.name reg) in
    Constructor.reg find

end
