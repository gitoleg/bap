open OUnit2
open Core_kernel.Std

open Bap_types.Std
open Bap_trace_meta_types
open Bap_trace_event_types
open Bap_trace_events


let move = Move.({cell = Word.b1; data = Word.b1;})
let var = Var.create ~tmp:true "temp" (Type.Imm 0x1) 
let reg = Move.({cell = var; data = Word.b1;})

let set_attr t attr a ctxt = 
  let t = Bap_trace.set_attr t attr a in
  match Bap_trace.get_attr t attr with
  | None -> assert_failure "get attribute failed"
  | Some a' ->
    assert_equal ~ctxt ~cmp:Value.equal a a'

let has_attr t attr a ctxt = 
  let t = Bap_trace.set_attr t attr a in
  let has = Bap_trace.has_attr t attr in
  assert_bool "attribute is absent" has

let set_meta t dict ctxt =
  let t = Bap_trace.set_meta t dict in
  let dict' = Bap_trace.meta t in 
  let cmp d d' = Value.Dict.compare d d' = 0 in
  assert_equal ~ctxt ~cmp dict dict'

let add_event t tag v ctxt = 
  let t = Bap_trace.add_event t tag v in
  match Bap_trace.contains t tag with
  | Some s -> assert_bool "add event failed" s
  | None -> assert_failure "add event failed"

let find t tag v ctxt = 
  let t = Bap_trace.add_event t tag v in
  match Bap_trace.find t tag with 
  | None -> assert_failure "find failed" 
  | Some v' -> assert_equal ~ctxt v v'

let find_all t count tag v ctxt = 
  let rec add n t = 
    if n < count then
      Bap_trace.add_event t tag v |> add (n + 1)
    else t in
  let t = add 0 t in
  let s = Bap_trace.find_all t tag in
  let s = Seq.filter s ~f:(fun v' -> v = v') in
  let len = Seq.length s in
  assert_equal ~ctxt len count

let find_all_matching t ctxt = 
  let t = Bap_trace.add_event t memory_load move in
  let t = Bap_trace.add_event t memory_store move in
  let t = Bap_trace.add_event t register_read reg in
  let t = Bap_trace.add_event t register_write reg in
  let s = Bap_trace.find_all_matching t
          Value.Match.(begin
              case memory_load  (fun x  -> `Memory) @@
              case memory_store (fun x  -> `Memory) @@
              default           (fun () -> `Unknown)
            end) in
  let is_memory p = p = `Memory in
  let s' = Seq.filter s ~f:is_memory in
  assert_bool "failed find_all_matching"
    (Seq.length s = Seq.length (Bap_trace.events t) &&
     Seq.length s' > 0)

let fold_matching t ctxt = 
  let t = Bap_trace.add_event t memory_load move in
  let t = Bap_trace.add_event t memory_store move in
  let t = Bap_trace.add_event t register_read reg in
  let t = Bap_trace.add_event t register_write reg in
  let s = Bap_trace.fold_matching t ~init:[] ~f:(fun xs x -> xs @ x)
          Value.Match.(begin
              case memory_load  (fun x  -> [`Memory,x]) @@
              case memory_store (fun x  -> [`Memory,x]) @@
              default           (fun () -> [])
            end) in
  let is_memory (p,_) = p = `Memory in
  assert_bool "failed fold_matching"
    (List.for_all s ~f:is_memory && List.length s > 0)

let append t ctxt =   
  let c = Value.create in
  let t = Bap_trace.add_event t register_read reg in
  let t = Bap_trace.add_event t register_write reg in
  let evs = Seq.of_list [c register_read reg; c register_write reg] in
  let evs' = 
    let ev = c memory_load move in
    Seq.of_list [ev; ev; ev;] in
  let expected = Seq.append evs evs' in
  let t = Bap_trace.append t evs' in
  let events = Bap_trace.events t in
  let cmp s s' = Seq.compare Value.compare s s' = 0 in
  assert_equal ~cmp events expected

let tool = Bap_trace.register_tool 
    ~name:"empty_tool" 
    ~supports:(fun _ -> false) 

let empty () = Bap_trace.create tool

let suite =  
  "Trace" >:::
  [
    "add_event"         >:: add_event (empty ()) memory_load move;
    "find"              >:: find (empty ()) memory_load move;
    "find_all"          >:: find_all (empty ()) 5 memory_load move;
    "find_all_matching" >:: find_all_matching (empty ());
    "fold_matching"     >:: fold_matching (empty ());
    "append"            >:: append (empty ());
  ]
