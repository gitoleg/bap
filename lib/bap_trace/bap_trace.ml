
open Core_kernel.Std
open Result
open Bap_types.Std

type event = value  with bin_io, sexp, compare
type id    = string with bin_io, compare, sexp
type proto = string
type tool  = string 

(** TODO: ask, if I able to change [next] interface to option *)
module Reader = struct
  type t = {
    tool : tool; 
    meta : dict; 
    next : unit -> event;
  }
end

type reader = Reader.t

type events = {
  stream : (unit -> event option) option;
  loaded : event Queue.t
}

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

let protocols_of_uri uri = 
  Hashtbl.fold protos ~init:[] 
    ~f:(fun ~key ~data acc -> 
        if data.probe uri then key::acc
        else acc) 

let io_with_proto uri (f : proto -> ('a, error) Result.t) = 
  match protocols_of_uri uri with
  | [] -> Error `No_provider
  | p::[] -> f p 
  | protos -> Error `Ambiguous_uri 

let make_events stream = {stream; loaded = Queue.create ();}

(** TODO: error generation should be much more clear and full *)
let load uri = 
  let load_with_proto p = 
    let create = Hashtbl.find_exn readers p in
    let id = make_id () in
    let reader = create uri id in
    (** TODO: wtf ? trace should give tool and proto, not a reader *)
    let tool = Reader.(reader.tool) in 
    let meta = Reader.(reader.meta) in
    let next () = Some (Reader.(reader.next ())) in
    let events = make_events (Some next) in
    Ok {id; meta; tool; events; proto = Some p} in
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
  events = make_events None;
  tool; 
  proto = None;
}

(** TODO: I bet f should return some new 'a too. Ask what the function
    behavior  *)
let unfold tool ~f ~init = 
  let next () = f init in
  let id = make_id () in
  let meta = Dict.empty in
  let proto = None in
  let events = make_events (Some next) in
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
let add_loaded t ev = Queue.enqueue t.events.loaded ev
  
let supports_by_tool: t -> 'a tag -> bool = fun t tag ->
  let f = Hashtbl.find_exn tools t.tool in
  f tag

let supports_by_proto: t -> 'a tag -> bool = fun t tag ->
  match t.proto with 
  | None -> false
  | Some proto ->
    let ops = Hashtbl.find_exn protos proto in
    ops.supports tag

let supports t tag = supports_by_tool t tag || supports_by_proto t tag

let of_stream t = match t.events.stream with
  | None -> fun () -> None
  | Some next ->
    fun () -> 
      let s = next () in
      match s with 
      | Some ev -> 
        let () = add_loaded t ev in
        s
      | None -> None

let memoize t = match t.events.stream with
  | None -> t
  | Some next -> 
    let rec load = function 
      | None -> t 
      | Some ev ->
        let () = add_loaded t ev in
        load (next ()) in
    load (next ())

let events t = 
  let evs = Seq.of_list (Queue.to_list t.events.loaded) in
  let f = of_stream t in
  let f' () = match f () with 
    | Some v -> Some (v, ())
    | None -> None in
  let evs' = Seq.unfold ~init:() ~f:f' in
  Seq.append evs evs'

(** TODO: ask, if we need event itself or its contains *)
let find t tag = 
  match Seq.find (events t) ~f:(Value.is tag) with
  | None -> None
  | Some ev -> Some (Value.get_exn tag ev)

let find_all t tag = 
  Seq.filter (events t) ~f:(Value.is tag)
 
(** TODO: ask about returing value: 'a or 'b *)
let fold_matching t matcher ~f ~init =
  let f' acc ev = f acc (Value.Match.switch ev matcher) in
  Seq.fold (events t) ~init ~f:f' 

let find_all_matching t matcher = 
  Seq.map ~f:(Value.Match.select matcher) (events t)

let contains t tag = 
  let in_trace = Seq.exists (events t) ~f:(Value.is tag) in
  if in_trace then Some true
  else if supports t tag then Some false
  else None

let add_event t tag e = 
  let ev = Value.create tag e in
  let () = add_loaded t ev in
  t

let append t evs = 
  let add evs = Seq.iter evs (fun ev -> add_loaded t ev) in
  let next' () = 
    let next = t.events.stream in
    match next with
    | None -> add evs; None
    | Some f -> f () in    
  let events = { t.events with stream = Some next' } in
  {t with events}

let add tab key data = Hashtbl.add_exn tab ~key ~data
let register_tool ~name ~supports = add tools name supports; name
let register_reader proto init = add readers proto init
let register_writer proto write = add writers proto write
let register_proto ~name ~probe ~supports = 
  add protos name {probe; supports}; name

