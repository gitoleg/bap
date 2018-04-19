open Core_kernel.Std
open Bap.Std
open Bap_rtl_types

module Exp = Bap_rtl_exp.Exp
open Bap_rtl_bitwidth

let zero = Exp.of_word Word.b0
let one  = Exp.of_word Word.b1
let ones = Exp.pattern Word.b1

let last e bits =
  let w = Exp.width e in
  Exp.extract (w - 1) (w - bits) e

let first e bits = Exp.extract (bits - 1) 0 e

let high w e = last e (int_of_bitwidth w)
let low w e = first e (int_of_bitwidth w)

let msb e =
  let h = Exp.width e - 1 in
  Exp.extract h h e

let lsb e = Exp.extract 0 0 e

let nth w e index =
  let step = int_of_bitwidth w in
  let hi = (index + 1) * step - 1 in
  let lo = index * step in
  Exp.extract hi lo e

module Normalize = struct
  open Bap.Std

  let jmp_exists bil =
    (object inherit [unit] Stmt.finder
      method! enter_jmp _ r = r.return (Some ())
    end)#find bil |> Option.is_some

  let remove_if bil =
    let open Bil in
    let product xs ys = match xs,ys with
      | x, [] | [], x -> x
      | xs, ys ->
        List.fold xs ~init:[]
          ~f:(fun acc x ->
              List.fold ys ~init:acc
                ~f:(fun acc y -> (y @ x) :: acc)) in
    let rec loop acc n = function
      | [] -> acc, n
      | If (_e, ts, es) :: bil ->
        let ts,n = loop [] n ts in
        let es,n = loop [] n es in
        let acc1 = product acc ts in
        let acc2 = product acc es in
        loop (acc1 @ acc2) n bil
      | x :: bil ->
        match acc with
        | [] -> loop [[n,x] ] (succ n) bil
        | _ ->
        let acc = List.map acc ~f:(fun xs -> (n, x) :: xs) in
        loop acc (succ n) bil in
    let vars,_ = loop [] 0 bil in
    List.map ~f:List.rev vars

  let replace_jmp line addr bil =
    let open Bil in
    let rec loop acc n = function
      | [] -> List.rev acc,n
      | Jmp _ :: bil when Int.equal n line ->
        let acc = (Jmp (Int addr)) :: acc in
        loop acc (succ n) bil
      | If (e, ts, es) :: bil ->
        let ts,n = loop [] n ts in
        let es,n = loop [] n es in
        let acc = (If (e, ts,es)) :: acc in
        loop acc n bil
      | x :: bil -> loop (x :: acc) (succ n) bil in
    let bil, _ = loop [] 0 bil in
    bil

  let norm_jumps bil =
    if not (jmp_exists bil) then bil
    else
      let get_bil lines = List.map lines ~f:snd in
      let jmp_line lines =
        List.find_map_exn lines ~f:(fun (n, s) ->
            match s with
            | Bil.Jmp _ -> Some n
            | _ -> None) in
      let jmps =
        remove_if bil |>
        List.filter ~f:(fun lines ->
            jmp_exists (get_bil lines)) |>
        List.filter_map ~f:(fun lines ->
            let n = jmp_line lines in
            let c = Stmt.eval (get_bil lines) (new Bili.context) in
            match c#pc with
            | Bil.Imm a -> Some (n, a)
            | _ -> None) in
      match jmps with
      | [] -> bil
      | (n,x) :: [] -> replace_jmp n x bil
      | jmps ->
        let jmps = List.sort jmps
            ~cmp:(fun (n,_) (m,_) -> Int.compare n m) in
        let jmps = List.group jmps ~break:(fun (n,_) (m,_) -> n <> m) in
        List.fold jmps ~init:bil ~f:(fun bil -> function
            | [] -> bil
            | (line, jmp) :: jmps when List.for_all jmps
                  ~f:(fun (m,j) -> m = line && Word.equal jmp j) ->
              replace_jmp line jmp bil
            | _ -> bil)
end

let norm_jumps = Normalize.norm_jumps
