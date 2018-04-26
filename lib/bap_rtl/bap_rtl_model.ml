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

  type 'a load = 'a exp -> bitwidth -> rhs exp
  type ('a, 'b) store = 'a exp -> 'b exp -> bitwidth -> rtl

  let load mem endian =
    fun addr width ->
      let size = size_of_width width in
      Exp.load mem addr endian size

  let store mem endian =
    fun addr data width ->
      let size = size_of_width width in
      store mem addr data endian size

end

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

module Cls = struct
  type t = string [@@deriving bin_io,compare,sexp]

  let of_string = ident

  let gpr = "gpr"
  let fpr = "fpr"
  let vector = "vector"
  let system = "system"
  let flag = "flag"

  let equal = String.equal
  let pp fmt t = Format.fprintf fmt "%s" t
end

type cls = Cls.t [@@deriving bin_io,compare,sexp]

(* register id  *)
type rid = {
  cls : Cls.t;
  name : Name.t;
} [@@deriving bin_io,compare,sexp]

module Rid = struct
  type t = rid  [@@deriving bin_io,compare,sexp]
  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io,compare,sexp]

      let module_name = Some "Model.Reg.Rid"
      let hash = Hashtbl.hash
      let version = "0.1"
      let pp fmt t =
        Format.fprintf fmt "%a%a" Cls.pp t.cls Name.pp t.name

    end)
end

module Reg = struct

  type name = Name.t [@@deriving bin_io,compare,sexp]

  type data = {
    exp : lhs exp;
    var : var option;
  }

  type t = data Rid.Map.t

  let empty = Rid.Map.empty

  let add cls ?(aliases=[]) var t =
    let names = `Name (Var.name var) :: aliases in
    let data = {var = Some var; exp = Exp.of_var var;} in
    List.fold names ~init:t
      ~f:(fun t name ->
          let rid = {cls; name} in
          Map.add t rid data)

  let search t cls n = match cls with
    | Some cls -> Map.find t {cls; name=n}
    | None ->
      List.find_map (Map.to_alist t)
        ~f:(fun ({name}, data) ->
            Option.some_if (Name.equal name n) data)

  let get_reg = function
    | Some {var} -> var
    | _ -> None

  let get_exp = function
    | Some {exp} -> Some exp
    | _ -> None

  let find  t ?cls n = search t cls n |> get_reg

  module Exn = struct

    let er_msg what = function
      | `Index i -> sprintf "%s not found by index %d" what i
      | `Name n -> sprintf "%s not found by name %s" what n

    let get_reg x = function
      | Some {var} when Option.is_some var ->
        Option.value_exn var
      | _ -> failwith (er_msg "register" x)

    let get_exp x = function
      | Some {exp} -> exp
      | _ -> failwith (er_msg "expression" x)

    let find_reg t cls n =
      search t cls n |> get_reg n

    let find_exp t cls n =
      search t cls n |> get_exp n

    let reg  t ?cls n = find_reg t cls n
    let exp  t ?cls n = find_exp t cls n
  end

  let find_exn = Exn.reg

  let ec t =
    let find reg = Exn.exp t (`Name (Reg.name reg)) in
    Constructor.reg find

  let all (t : t) cls =
    Map.filter_mapi t ~f:(fun ~key ~data ->
        if Cls.equal key.cls cls then data.var
        else None) |> Map.data

  module Exp = struct

    let add cls ?(aliases=[]) name exp t =
      List.fold (name :: aliases) ~init:t
        ~f:(fun t name ->
            Map.update t {cls; name} ~f:(function
                | None -> {var=None; exp}
                | Some d -> {d with exp}))

    let find t ?cls n = search t cls n |> get_exp

    let all (t : t) cls =
      Map.filter_mapi t ~f:(fun ~key ~data ->
          Option.some_if (Cls.equal key.cls cls) data.exp) |>
      Map.data

    let find_exn = Exn.exp

  end
end
