open Core_kernel.Std
open Bap.Std
open Bap_rtl_kernel
open Bap_rtl_helpers

let size_of_width x =
  let x = int_of_bitwidth x in
  match Size.of_int x with
  | Ok s -> s
  | Error _ -> failwith (sprintf "invalid size: %d" x)

module type M = sig
  val mem : var
  val endian : endian
end

type cls = string [@@deriving bin_io,compare,sexp]
exception Register_not_found of string

module Reg_class = struct
  type t = cls [@@deriving bin_io,compare,sexp]

  let gpr = "GPR"
  let fpr = "FPR"
  let flag = "FLAG"
  let vector = "VECTOR"

  let create = ident

end

module Make(M : M) = struct
  open M

  let load addr width =
    let size = size_of_width width in
    Exp.load mem addr endian size

  let store addr data width =
    let size = size_of_width width in
    store mem addr data endian size

  type view = {
    names : exp String.Table.t;
    numbs : exp Int.Table.t;
  }

  let create_view () = {
    names = String.Table.create ();
    numbs = Int.Table.create ();
  }

  let classes = String.Table.create ()

  let find_view cls =
    Hashtbl.find_or_add classes cls ~default:create_view

  let add_exp name exp ?(aliases = []) ?index cls =
    let view = find_view cls in
    List.iter (name :: aliases) ~f:(fun name ->
        Hashtbl.change view.names name (function
            | _ -> Some exp));
    Option.iter index ~f:(fun num ->
        Hashtbl.change view.numbs num (function
            | _ -> Some exp))

  let add_reg name width ?(aliases = []) ?index cls =
    let var = Var.create name (Type.Imm width) in
    add_exp name (Exp.of_var var) ~aliases ?index cls

  let add_var var ?(aliases = []) ?index cls =
    add_exp (Var.name var) (Exp.of_var var) ~aliases ?index cls

  let reg_not_found x =
    let errs = match x with
      | `Name n -> sprintf "Register %s not found" n
      | `Numb n -> sprintf "Register %d not found" n in
    raise (Register_not_found errs)

  let find cls reg =
    let view = find_view cls in
    let name = Reg.name reg in
    match Hashtbl.find view.names name with
    | Some e -> e
    | None -> reg_not_found (`Name name)

  let findi cls num =
    let view = find_view cls in
    match Hashtbl.find view.numbs num with
    | Some e -> e
    | None -> reg_not_found (`Numb num)

end
