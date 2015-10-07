open OUnit2
open Core_kernel.Std
open Or_error

open Bap_types.Std
module Trace = Bap_trace_std.Trace
open Trace.Event
open Trace.Meta
 
let () = Bap_trace_binprot.register ()

let bed = Word.of_int32 0xBEDl
let coffee = Word.of_int32 0xC0FFEEl
let move = Trace.Move.({cell = bed; data = coffee;})
let var = Var.create ~tmp:true "temp" (Type.Imm 0xC0FFEE) 
let reg = Trace.Move.({cell = var; data = coffee;})
let bin = Trace.Binary.({path="/dev/null"; arch=`x86; stripped = None})
let trc = Trace.Tracer.({name="test_tracer"; args=Array.empty (); version="";}) 

let test_events = 
  [ Value.create memory_load move;
    Value.create memory_store move;
    Value.create register_read reg;
    Value.create register_write reg; ]

let test_meta = 
  let open Dict in
  let meta = set empty binary bin in
  set meta tracer trc   

module Tool : Trace.S = struct
  let name = "test_tool" 
  let supports = fun tag -> 
    let same t = Value.Tag.same tag t in
    same memory_load || same memory_store
end

let test_tool = Trace.register_tool (module Tool)
let empty = Trace.create test_tool

let assert_seq_equal ~ctxt  =
  let cmp s s' = Seq.compare Value.compare s s' = 0 in
  assert_equal ~ctxt ~cmp 

let assert_dict_equal ~ctxt = 
  let cmp d d' = Value.Dict.compare d d' = 0 in
  assert_equal ~ctxt ~cmp

let save_and_load ctxt =
  let uri = Uri.of_string "file:///tmp/bap_trace.test" in
  let save () =
    let t = Trace.append empty (Seq.of_list test_events) in
    let t = Trace.set_meta t test_meta in
    let r = Trace.save uri t in
    assert_bool "save failed" (Result.is_ok r) in
  let load () = 
    match Trace.load uri with
    | Ok t -> 
      let evs = Trace.events t in
      let meta = Trace.meta t in
      let tool = Trace.tool t in
      assert_seq_equal ~ctxt evs (Seq.of_list test_events);
      assert_dict_equal ~ctxt meta test_meta;
      assert_equal ~ctxt test_tool tool
    | Error s -> assert_failure "load failed" in
  save ();
  load ()

let id ctxt = 
  let t  = Trace.create test_tool in
  let t' = Trace.create test_tool in
  let id,id' = Trace.(id t, id t') in
  let not_eql = not (Trace.Id.equal id id') in
  assert_bool "id mustn't be equal, failed" not_eql

let trace_tool ctxt = 
  let t = Trace.create test_tool in
  let tool = Trace.tool t in
  assert_equal ~ctxt tool test_tool

let set_and_get_attr t attr a ctxt = 
  let t = Trace.set_attr t attr a in
  match Trace.get_attr t attr with
  | None -> assert_failure "get attribute failed"
  | Some a' -> assert_equal ~ctxt a a'

let has_attr t attr a ctxt = 
  let t = Trace.set_attr t attr a in
  let has = Trace.has_attr t attr in
  assert_bool "attribute is absent" has

let set_meta t ctxt =
  let open Value.Dict in 
  let t = Trace.set_meta t test_meta in
  let meta = Trace.meta t in 
  assert_dict_equal ~ctxt meta test_meta

let add_event t tag v ctxt = 
  let t = Trace.add_event t tag v in
  match Trace.contains t tag with
  | Some s -> assert_bool "add event failed" s
  | None -> assert_failure "add event failed"

let events ctxt = 
  let evs = Seq.of_list test_events in
  let t = Trace.append empty evs in
  let evs' = Trace.events t in
  assert_seq_equal ~ctxt evs evs'

let find t tag v ctxt = 
  let t = Trace.add_event t tag v in
  match Trace.find t tag with 
  | None -> assert_failure "find failed" 
  | Some v' -> assert_equal ~ctxt v v'

let find_all count tag value ctxt = 
  let rec add n t = 
    if n < count then
      Trace.add_event t tag value |> add (n + 1)
    else t in
  let t = add 0 empty in
  let s = Trace.find_all t tag in
  let s = Seq.filter s ~f:(fun v -> v = value) in
  assert_equal ~ctxt (Seq.length s) count

let add_list t evs = 
  List.fold_left evs ~init:empty 
    ~f:(fun t (tag,v) -> Trace.add_event t tag v)

let find_all_matching ctxt = 
  let memory_events = [memory_load,move; memory_store,move] in
  let t = add_list empty memory_events in
  let t = Trace.add_event t register_read reg in
  let t = Trace.add_event t register_write reg in
  let s = Trace.find_all_matching t
          Value.Match.(begin
              case memory_load  (fun x  -> `Memory) @@
              case memory_store (fun x  -> `Memory) @@
              default           (fun () -> `Unknown)
            end) in
  let s' = Seq.filter s ~f:(fun x -> x = `Memory) in
  assert_bool "failed find_all_matching"
    (Seq.length s' = List.length memory_events)

let fold_matching ctxt = 
  let memory_events = [memory_load,move; memory_store,move] in
  let t = add_list empty memory_events in
  let t = Trace.add_event t register_read reg in
  let t = Trace.add_event t register_write reg in
  let s = Trace.fold_matching t ~init:[] ~f:(fun xs x -> xs @ x)
          Value.Match.(begin
              case memory_load  (fun x  -> [`Memory,x]) @@
              case memory_store (fun x  -> [`Memory,x]) @@
              default           (fun () -> [])
            end) in
  let is_memory (p,_) = p = `Memory in
  assert_bool "failed fold_matching"
    (List.for_all s ~f:is_memory && 
     List.length s = List.length memory_events)

let supports ctxt = 
  let m = Trace.supports empty memory_load in
  let m' = Trace.supports empty memory_store in
  let r = not (Trace.supports empty register_read) in
  assert_bool "supports failed" (m && m' && r)

let contains t ctxt = 
  let t = Trace.add_event t memory_load move in
  match Trace.contains t memory_load with
  | Some r -> assert_bool "contains failed" r
  | None -> assert_failure "contains failed"

let append ctxt =
  let t = empty in
  let t = Trace.add_event t register_read reg in
  let t = Trace.add_event t register_write reg in
  let c = Value.create in
  let evs = [c register_read reg; c register_write reg] in
  let expected = Seq.of_list (evs @ test_events) in
  let t = Trace.append t (Seq.of_list test_events) in
  let events = Trace.events t in
  assert_seq_equal ~ctxt events expected

let next () = 
    let a = ref test_events in
    fun () -> match !a with
      | [] -> None
      | hd::tl -> a := tl; Some hd 

let unfold ctxt = 
  let t = Trace.unfold test_tool ~f:(next ()) ~init:() in
  let evs = Trace.events t in
  let evs' = Seq.of_list test_events in
  assert_seq_equal ~ctxt evs evs'

let check_calls_order ctxt = 
  let t = Trace.unfold test_tool ~f:(next ()) ~init:() in
  let evs = Trace.events t in
  let _ = Trace.find t register_read in
  let evs' = Trace.events t in
  assert_seq_equal ~ctxt evs evs'

let memoize ctxt = 
  let a = ref 0 in
  let next = 
    let evs = ref test_events in
    fun () -> match !evs with
      | [] -> None
      | hd::tl -> 
        a := !a + 1; evs := tl; Some hd in
  let t = Trace.unfold test_tool ~f:next ~init:() in
  assert_equal ~ctxt !a 0;
  let _ = Trace.memoize t in
  assert_equal ~ctxt !a (List.length test_events)

let suite =  
  "Trace" >:::
  [
    "save_and_load"     >:: save_and_load;
    "id"                >:: id;
    "tool"              >:: trace_tool;
    "set_and_get_attr"  >:: set_and_get_attr empty binary bin;
    "has_attr"          >:: has_attr empty binary bin;
    "set_meta"          >:: set_meta empty;
    "add_event"         >:: add_event empty memory_load move;
    "events"            >:: events;
    "find"              >:: find empty memory_load move;
    "find_all"          >:: find_all 5 memory_load move;
    "find_all_matching" >:: find_all_matching;
    "fold_matching"     >:: fold_matching;
    "contains"          >:: contains empty;
    "supports"          >:: supports;
    "append"            >:: append;
    "unfold"            >:: unfold;
    "check_calls"       >:: check_calls_order;
    "memoize"           >:: memoize;
  ]
