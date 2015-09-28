
open Core_kernel.Std
open Result
open Bap.Std

type event = value  with bin_io, sexp, compare
type id    = string with bin_io, compare, sexp
type proto = string
type tool  = string 

module Reader = struct
  type t = {
    tool : tool; 
    meta : dict; 
    next : unit -> event;
  }
end

type reader = Reader.t

(** TODO: think here about type representation.
    It seems sufficient to have only one branch 
    with event Bap_seq.t *)
type events =
  | Stream of event seq
  | Loaded of event list

module type S = module type of String.Caseless.Table

module Tab = String.Caseless.Table

type 'a tools = ('a tag -> bool) Tab.t

type 'a proto_ops = { 
  probe: (Uri.t -> bool);
  supports: 'a tag -> bool;
}

type 'a protos = ('a proto_ops) Tab.t

type t = {
  id     : id;
  meta   : dict;
  events : events;
  tool   : tool;
  proto  : proto option;
}

type readers = (Uri.t -> id -> reader) Tab.t
type writers = (Uri.t -> t -> unit Or_error.t) Tab.t

let mk_tab = Tab.create
let tools  : 'a tools  = mk_tab () 
let protos : 'a protos = mk_tab () 
let readers: readers  = mk_tab ()
let writers: writers  = mk_tab ()

let make_id () = Uuidm.(to_string (create `V4))

module Id = Regular.Make(struct
    type t = id with bin_io, compare, sexp

    let hash = Hashtbl.hash
    let pp fmt t = Format.fprintf fmt "%s" t (** TODO  *)
    let module_name = Some "Bap_trace.Id"
  end) 

type error = [
  | `No_provider    (** No provider for a given URI               *)
  | `Ambiguous_uri  (** More than one provider for a given URI    *)
  | `Protocol_error of Error.t  (** Data encoding problem         *)
  | `System_error of Error.t    (** System error                  *)
]

let find_protocols uri = 
  Hashtbl.fold protos ~init:[] 
    ~f:(fun ~key ~data acc -> 
        if data.probe uri then key::acc
        else acc) 

let io_with_proto uri (f : proto -> ('a, error) Result.t) = 
  match find_protocols uri with
  | [] -> Error `No_provider
  | p::[] -> f p 
  | protos -> Error `Ambiguous_uri 

(** TODO: error generation should be much more clear and full *)
let load uri = 
  let load_with_proto p = 
    let create = Hashtbl.find_exn readers p in
    let id = make_id () in
    let reader = create uri id in
    (** TODO: wtf ? trace should give tool and proto, not a reader *)
    let tool = Reader.(reader.tool) in 
    let meta = Reader.(reader.meta) in
    let evs = Seq.unfold ~init:Reader.(reader.next) (** add error check here *)
        ~f:(fun next -> Some (next (), next)) in
    Ok {id; meta; tool; events = Stream evs; proto = Some p} in
  io_with_proto uri load_with_proto

let save uri t = 
  let save_with_proto p : (unit, error) Result.t= 
    let write = Hashtbl.find_exn writers p in
    match write uri t with 
    | Ok () as r -> r
    | Error err -> Error (`System_error err) in
  io_with_proto uri save_with_proto

let create tool = {
  id = make_id (); 
  meta = Dict.empty; 
  events = Loaded [];
  tool; 
  proto = None;
}

(** TODO: I bet f should return some new 'a too *)
let unfold tool ~f ~init = 
  let f' a = match f a with 
    | Some ev -> Some (ev,a)
    | None -> None in
  let id = make_id () in
  let meta = Dict.empty in
  let proto = None in
  let events = Stream (Seq.unfold ~init ~f:f') in
  {id; meta; tool; events; proto}  

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
  
let supports_by_tool: t -> 'a tag -> bool = fun t tag ->
  let f = Hashtbl.find_exn tools t.tool in
  f tag

let supports_by_proto: t -> 'a tag -> bool = fun t tag ->
  match t.proto with 
  | None -> false
  | Some proto ->
    let ops = Hashtbl.find_exn protos proto in
    ops.supports tag

let supports t tag = 
  supports_by_tool t tag || supports_by_proto t tag

let memoize t = match t.events with
  | Loaded _ -> t
  | Stream events ->
    let forced = Seq.force_eagerly events in
    {t with events = Stream forced}

let of_loaded evs = Seq.of_list (List.rev evs)

(** TODO: ask, if we need event itself or its contains *)
let find t tag = 
  let ev = match t.events with
    | Loaded events -> List.find events ~f:(Value.is tag) 
    | Stream evs -> Seq.find evs ~f:(Value.is tag) in
  match ev with 
  | None -> None
  | Some ev -> Some (Value.get_exn tag ev)

let find_all t tag = 
  let evs = match t.events with
    | Loaded events ->
      let evs = List.filter events ~f:(Value.is tag) in
      of_loaded evs
    | Stream evs -> Seq.filter evs ~f:(Value.is tag) in
  Seq.map ~f:(Value.get_exn tag) evs
  
(** TODO: ask about returing value: 'a or 'b *)
let fold_matching t matcher ~f ~init =
  let evs = match t.events with 
    | Loaded evs -> of_loaded evs
    | Stream evs -> evs in
  let f' acc ev = f acc (Value.Match.switch ev matcher) in
  Seq.fold evs ~init ~f:f' 

let find_all_matching t matcher = 
  let seq = match t.events with 
    | Loaded events -> of_loaded events
    | Stream events -> events in
  Seq.map ~f:(Value.Match.select matcher) seq

let contains t tag = 
  let is = Value.is tag in
  let in_trace = match t.events with
    | Loaded evs -> List.exists ~f:is evs 
    | Stream evs -> Seq.exists ~f:is evs in
  if in_trace then Some true
  else if supports t tag then Some false
  else None

let events t = match t.events with
  | Loaded evs -> of_loaded evs
  | Stream evs -> evs
 
(** TODO: it's a question how to add event to stream   *)
let add_event t tag e = 
  let ev = Value.create tag e in
  match t.events with 
  | Loaded evs -> {t with events = Loaded (ev::evs)}
  | Stream evs -> {t with events = Stream (Seq.cons ev evs)}

let append t evs = match t.events with 
  | Loaded evs' -> 
    let evs' = of_loaded evs' in
    let evs = Seq.append evs' evs in
    {t with events = Loaded (Seq.to_list evs)}
  | Stream evs' -> {t with events = Stream (Seq.append evs evs')}

let add tab key data = Hashtbl.add_exn tab ~key ~data
let register_tool ~name ~supports = add tools name supports; name
let register_reader proto init = add readers proto init
let register_writer proto write = add writers proto write
let register_proto ~name ~probe ~supports = 
  add protos name {probe; supports}; name

