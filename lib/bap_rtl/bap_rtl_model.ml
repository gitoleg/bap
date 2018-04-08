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

  type name = Name.t [@@deriving bin_io,compare,sexp]

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

  type rid = {
      cls : cls;
      name : name;
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


  type data = {
    exp : exp;
    var : var option;
  }

  type t = data Rid.Table.t

  let create () = Rid.Table.create ()
  let update t key data = Hashtbl.update t key (fun _ -> data)

  let add_reg t cls ?(aliases=[]) var =
    let names = `Name (Var.name var) :: aliases in
    let data = {var = Some var; exp = Exp.of_var var;} in
    List.iter names
      ~f:(fun name ->
          let rid = {cls; name} in
          Hashtbl.update t rid (fun _ -> data))

  let add_reg' t cls ?(aliases=[]) name exp =
    List.iter (name :: aliases)
      ~f:(fun name ->
          let rid = {cls; name} in
          Hashtbl.update t rid (function
              | None -> {var=None; exp}
              | Some d -> {d with exp}))

  let find t cls n = match cls with
    | Some cls -> Hashtbl.find t {cls; name=n}
    | None ->
      List.find_map (Hashtbl.to_alist t)
        ~f:(fun ({name}, data) ->
            Option.some_if (Name.equal name n) data)



  let findi t cls i =
    Hashtbl.find t {cls; name = `Index i}

  let get_reg = function
    | Some {var} -> var
    | _ -> None

  let get_exp = function
    | Some {exp} -> Some exp
    | _ -> None

  let reg  t ?cls n = find t cls (`Name n) |> get_reg
  let exp  t ?cls n = find t cls (`Name n) |> get_exp
  let regi t cls i = find t (Some cls) (`Index i) |> get_reg
  let expi t cls i = find t (Some cls) (`Index i) |> get_exp

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
      find t cls n |> get_reg n

    let find_exp t cls n =
      find t cls n |> get_exp n

    let reg  t ?cls n = find_reg t cls (`Name n)
    let exp  t ?cls n = find_exp t cls (`Name n)
    let regi t cls i = find_reg t (Some cls) (`Index i)
    let expi t cls i = find_exp t (Some cls) (`Index i)
  end


  let reg_ec t =
    let find reg = Exn.exp t (Reg.name reg) in
    Constructor.reg find

end
