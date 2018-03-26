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
          | `Name n -> Format.fprintf fmt "%s" n
          | `Index i -> Format.fprintf fmt "%d" i

      end)
  end

  type data =
    | Var of var
    | Exp of exp

  type name = Name.t

  type t = data Name.Table.t

  let create () = Name.Table.create ()
  let change t key data = Hashtbl.change t key ~f:(fun _ -> Some data)
  let change' t keys data = List.iter ~f:(fun k -> change t k data) keys

  let add t ?(aliases=[]) name data =
    let names = name :: aliases in
    change' t names data

  let add' t ?(aliases=[]) name e =
    add t ~aliases name (Exp e)

  let add t ?(aliases=[]) reg =
    let name = `Name (Var.name reg) in
    add t ~aliases name (Var reg)

  let find t name = match Hashtbl.find t name with
    | Some (Var v) -> Some v
    | _ -> None

  let find_exp t name =
    match Hashtbl.find t name with
    | Some (Var v) -> Some (Exp.of_var v)
    | Some (Exp e) -> Some e
    | None -> None

  let find_reg t reg = find_exp t (`Name (Reg.name reg))
  let chain find ms x = List.find_map ms ~f:(fun m -> find m x)

  module Exn = struct

    let raise_on_none = function
      | Some x -> x
      | None -> raise Not_found

    let find t name = raise_on_none @@ find t name
    let find_reg t reg  = raise_on_none @@ find_reg t reg
    let find_exp t name = raise_on_none @@ find_exp t name

    let chain find ms x =
      let f m = Option.try_with (fun () -> find m x) in
      List.find_map ms ~f |> raise_on_none
  end

end
