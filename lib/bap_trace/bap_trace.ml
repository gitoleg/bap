
open Core_kernel.Std
open Result
open Bap_types.Std

type event = value  with bin_io, sexp, compare

type on_error = [
  | `Fail
  | `Miss
  | `Stop
  | `Pack of (Error.t -> event) 
  | `Warn of (Error.t -> unit)
]

type monitor = [
  | on_error
  | `User of (event Or_error.t seq -> event seq)
]

type proto = string
type tool  = string with bin_io, sexp

module Monitor = struct
  type t = monitor
  let fail_on_error   = `Fail
  let ignore_errors   = `Miss
  let stop_on_error   = `Stop
  let warn_on_error f = `Warn f
  let pack_errors f   = `Pack f
  let create f        = `User f
end

module Id = Bap_uuid
type id = Id.t with bin_io, compare, sexp

module type S = sig
  val name: string 
  val supports: 'a tag -> bool
end

module type P = sig
  include S
  val probe: Uri.t -> bool
end

type io_error = [ 
  | `Protocol_error of Error.t  (** Data encoding problem         *)
  | `System_error of Error.t    (** System error                  *)
] with sexp, bin_io, compare

type error = [
  | io_error
  | `No_provider    (** No provider for a given URI               *)
  | `Ambiguous_uri  (** More than one provider for a given URI    *)
] with sexp

module Reader = struct
  type t = {
    tool : tool; 
    meta : dict; 
    next : unit -> event Or_error.t option;  
  }
end

type reader = Reader.t

type stream = {
  data : event seq; 
  after: event seq;
} 

type events = | Loaded of event seq | Stream of stream

type t = {
  id     : id;
  meta   : dict;
  events : events;
  tool   : tool;
  proto  : proto option;
}

module Tab = String.Caseless.Table

let mk_tab = Tab.create
let tools  : (module S) Tab.t = mk_tab () 
let protos : (module P) Tab.t = mk_tab () 
let readers: (Uri.t -> id -> reader Or_error.t) Tab.t = mk_tab ()
let writers: (Uri.t -> t -> unit Or_error.t) Tab.t = mk_tab ()
let make_id () = Bap_uuid.create `V4

let protocols_of_uri uri = 
  Hashtbl.fold protos ~init:[] 
    ~f:(fun ~key ~data acc -> 
        let module A = (val data : P) in
        if A.probe uri then key::acc
        else acc) 

let find_proto uri = match protocols_of_uri uri with
  | [] -> Error `No_provider
  | p::[] -> Ok p 
  | protos -> Error `Ambiguous_uri 

let find_by_proto tab proto = 
  try 
    Ok (Hashtbl.find_exn tab proto)
  with Not_found -> Error `No_provider

let create_reader create uri id =
  match create uri id with
  | Ok rd as r -> r
  | Error err -> Error (`System_error err)

let make_stream next monitor = 
  let open Seq.Generator in
  let make m =
    let rec traverse () = match next () with
      | Some (Ok ev) -> yield ev
      | Some (Error er) -> of_error er
      | None -> return () and
    of_error = match m with
      | `Fail -> Error.raise 
      | `Miss -> fun err -> traverse ()
      | `Pack f -> fun err -> yield (f err) >>= traverse
      | `Stop -> fun err -> return () 
      | `Warn f -> fun err -> f err; traverse () in
    traverse in
  let rec traverse' () = match next () with
    | Some elt -> yield elt >>= traverse'
    | None -> return () in
  let s = match monitor with 
    | #on_error as m -> run (return () >>= make m)
    | `User f -> run (return () >>= traverse') |> f in
  let s = Seq.memoize s in
  Stream {data = s; after = Seq.empty }

let of_reader reader id proto monitor =  
  let tool = reader.Reader.tool in 
  let meta = reader.Reader.meta in
  let events = make_stream reader.Reader.next monitor in
  {id; meta; tool; events; proto = Some proto;}

let load ?(monitor=Monitor.fail_on_error) uri =  
  find_proto uri >>= 
  fun proto -> find_by_proto readers proto >>=
  fun create ->
  let id = make_id () in
  create_reader create uri id >>=
  fun reader -> Ok (of_reader reader id proto monitor)

let save uri t = 
  find_proto uri >>= 
  fun proto -> find_by_proto writers proto >>=
  fun write -> match write uri t with
  | Ok () as r -> r
  | Error err -> Error (`System_error err)

let create tool = {
  id = make_id (); 
  meta = Dict.empty; 
  events = Loaded Seq.empty;
  tool; 
  proto = None;
}

(** TODO: I bet f should return some new 'a too. Ask what the function
    behavior  *)
let unfold ?(monitor=Monitor.fail_on_error) tool ~f ~init = 
  let next () = match f init with
    | Some ev -> Some (Ok ev)
    | None -> None in
  let id = make_id () in
  let meta = Dict.empty in
  let proto = None in
  let events = make_stream next monitor in
  {id; meta; tool; events; proto;}  

let set_attr t attr v = 
  let meta = match Dict.add t.meta attr v with 
    | `Ok meta -> meta
    | `Duplicate -> Dict.change t.meta attr (fun _ -> (Some v)) in
  {t with meta}

let id t = t.id
let get_attr t = Dict.find t.meta 
let has_attr t = Dict.mem  t.meta 
let meta t = t.meta
let tool t = t.tool
let set_meta t meta = {t with meta}

let supports_by_tool t tag = 
  let t = Hashtbl.find_exn tools t.tool in
  let module Tool = (val t : S) in
  Tool.supports tag

let supports_by_proto t tag = match t.proto with
  | None -> true
  | Some p ->
    let p = Hashtbl.find_exn protos p in
    let module Proto = (val p : P) in
    Proto.supports tag

let supports t tag = supports_by_tool t tag && supports_by_proto t tag

let memoize t = match t.events with
  | Loaded _ -> t
  | Stream s -> 
    let data = Seq.force_eagerly s.data in
    { t with events = Stream ({s with data}) }

let events t = match t.events with
    | Loaded evs -> evs
    | Stream s -> Seq.append s.data s.after 

let find t tag = Seq.find_map (events t) ~f:(Value.get tag) 
let find_all t tag = Seq.filter_map (events t) ~f:(Value.get tag)

let fold_matching t matcher ~f ~init =
  let f' acc ev = f acc (Value.Match.switch ev matcher) in
  Seq.fold (events t) ~init ~f:f' 

let find_all_matching t matcher = 
  Seq.map ~f:(Value.Match.select matcher) (events t)

let contains t tag = 
  let in_trace = Seq.exists (events t) ~f:(Value.is tag) in
  if in_trace || supports t tag then Some in_trace
  else None

let add_event t tag e = 
  let ev = Value.create tag e in
  let add s ev = Seq.append s (Seq.singleton ev) in
  match t.events with
  | Loaded evs -> {t with events = Loaded (add evs ev)}
  | Stream s -> 
    let s' = {s with after = add s.after ev} in
    {t with events = Stream s'}

let append t evs = 
  let add s s' = Seq.append s s' in
  match t.events with 
  | Loaded evs' -> {t with events = Loaded (add evs' evs)}
  | Stream s -> 
    let s' = {s with after = add s.after evs} in
    {t with events = Stream s'}

let add tab key data = Hashtbl.add_exn tab ~key ~data
let register_reader proto init = add readers proto init
let register_writer proto write = add writers proto write

let register_tool : (module S) -> tool = fun s ->
  let module A = (val s : S) in
  add tools A.name s; A.name
  
let register_proto : (module P) -> proto = fun p -> 
  let module A = (val p : P) in
  add protos A.name p; A.name


