open Bap_core_theory

module OCamlGraph = Graph

open Core_kernel
open Graphlib.Std

open Bap_types.Std
open Bap_image_std

open KB.Syntax

module Driver = Bap_disasm_driver
module Insn = Bap_disasm_insn

let single = Set.singleton (module Addr)

module Callgraph = struct
  let entry = Word.b0
  let is_entry = Word.equal entry
  include Graphlib.Make(Addr)(Unit)
  let mark_as_root n g =
    if Word.equal n entry then g
    else
      let e = Edge.create entry n () in
      Edge.insert e g
end

module Parents = struct
  let none = single Callgraph.entry
  let equal = Addr.Set.equal
  let is_root p = Set.mem p Callgraph.entry
  let merge = Set.union

  let transfer self parents =
    if is_root parents then single self
    else parents
end

module Find_parents = struct
  type t = (word,Word.Set.t) Solution.t
  include Binable.Of_binable(struct
      type t = (word * Word.Set.t) Seq.t [@@deriving bin_io]
    end)(struct
      type t = (word,Word.Set.t) Solution.t
      let to_binable = Solution.enum
      let of_binable xs =
        let init = ok_exn @@
          Map.of_increasing_sequence
            (module Word) xs in
        Solution.create init Parents.none
    end)
end

type input = Driver.state
type output = {
  parents : Find_parents.t;
  entries : Addr.Set.t;
} [@@deriving bin_io]

type t = output [@@deriving bin_io]

let string_of_node n =
  sprintf "%S" @@ if Callgraph.is_entry n
  then "entry"
  else Addr.string_of_value n

let pp_callgraph ppf graph =
  Graphlib.to_dot (module Callgraph) graph
    ~formatter:ppf
    ~string_of_node


let pp_roots ppf graph =
  Graphlib.to_dot (module Callgraph) graph
    ~formatter:ppf
    ~string_of_node:(fun s ->
        sprintf "%S" (Addr.string_of_value s))

let of_disasm disasm =
  Driver.explore disasm ~init:Callgraph.empty
    ~block:(fun mem _ -> KB.return (Memory.min_addr mem))
    ~node:(fun n g ->
        let g = Callgraph.Node.insert n g in
        Theory.Label.for_addr (Word.to_bitvec n) >>= fun code ->
        KB.collect Theory.Label.is_subroutine code >>| function
        | Some true -> Callgraph.mark_as_root n g
        | _ -> g)
    ~edge:(fun src dst g ->
        let e = Callgraph.Edge.create src dst () in
        KB.return (Callgraph.Edge.insert e g))


let empty =
  let root =
    Map.singleton (module Addr) Callgraph.entry Parents.none in {
    parents = Solution.create root Parents.none;
    entries = Set.empty (module Addr);
  }

let connect_inputs g =
  Callgraph.nodes g |>
  Seq.fold ~init:g ~f:(fun g n ->
      if Callgraph.Node.degree ~dir:`In n g = 0
      then Callgraph.mark_as_root n g
      else g)

let connect_unreachable_scc g =
  Graphlib.depth_first_search (module Callgraph) g
    ~start:Callgraph.entry
    ~init:g
    ~start_tree:Callgraph.mark_as_root

let callgraph disasm =
  of_disasm disasm >>|
  connect_inputs >>|
  connect_unreachable_scc

let entries graph =
  Callgraph.Node.outputs Callgraph.entry graph |>
    Seq.fold ~init:(Set.empty (module Addr))
      ~f:(fun entries e -> Set.add entries (Callgraph.Edge.dst e))

let pp_calls ppf (parents,graph) =
  let is_root addr =
    Set.is_empty (Solution.get parents addr) in
  Graphlib.to_dot (module Callgraph) graph
    ~formatter:ppf
    ~string_of_node
    ~node_attrs:(fun n ->
        if is_root n
        then [`Shape `Diamond; `Style `Filled]
        else [])

let update {parents} disasm =
  callgraph disasm >>| fun graph ->
  Graphlib.fixpoint (module Callgraph) graph
    ~init:parents
    ~start:Callgraph.entry
    ~equal:Parents.equal
    ~merge:Parents.merge
    ~f:Parents.transfer
  |> fun parents ->
  {
    parents;
    entries = entries graph;
  }

let common_entry {parents} a a' =
  not @@
  Set.is_empty @@
  Set.inter
    (Solution.get parents a)
    (Solution.get parents a')

let entries {entries} = entries

let equal s1 s2 =
  Set.equal s1.entries s2.entries &&
  Solution.equal ~equal:Parents.equal s1.parents s2.parents


let domain = KB.Domain.flat ~empty ~equal "callgraph"
