open OUnit2
open Core_kernel.Std
open Or_error

open Bap_types.Std
open Bap_trace_meta_types
open Bap_trace_meta
open Bap_trace_event_types
open Bap_trace_events

let () = Bap_trace_binprot.register ()

let move = Move.({cell = Word.b1; data = Word.b1;})
let var = Var.create ~tmp:true "temp" (Type.Imm 0x1) 
let reg = Move.({cell = var; data = Word.b1;})
let bin = Binary.({path="/dev/null"; arch=`x86; stripped = None})
let trc = Tracer.({name="test_tracer"; args=Array.empty (); version="";}) 

let test_events = 
  let c = Value.create in
  [ c memory_load move;
    c memory_store move;
    c register_read reg;
    c register_write reg;
    c pc_update Word.b1; ]

let test_meta = 
  let open Dict in
  let meta = set empty binary bin in
  set meta tracer trc   

module Tool : Bap_trace.S = struct
  let name = "test_tool" 
  let supports = fun tag -> 
    let same t = Value.Tag.same tag t in
    same memory_load || same memory_store

end

let test_tool = Bap_trace.register_tool (module Tool)

let empty = Bap_trace.create test_tool

let seq_eql s s' = Seq.compare Value.compare s s' = 0
let dict_eql d d' = Value.Dict.compare d d' = 0 

let save_and_load ctxt =
  let uri = Uri.of_string "file:///tmp/bap_trace.test" in
  let save () =
    let t = Bap_trace.append empty (Seq.of_list test_events) in
    let t = Bap_trace.set_meta t test_meta in
    let r = Bap_trace.save uri t in
    assert_equal ~ctxt (Ok ()) r in
  let load () = 
    match Bap_trace.load uri with
    | Ok t -> 
      let evs = Bap_trace.events t in
      let meta = Bap_trace.meta t in
      let tool = Bap_trace.tool t in
      assert_equal ~ctxt ~cmp:seq_eql evs (Seq.of_list test_events);
      assert_equal ~ctxt ~cmp:dict_eql meta test_meta;
      assert_equal ~ctxt test_tool tool
    | Error s -> assert_failure "load failed" in
  save ();
  load ()

let id ctxt = 
  let t  = Bap_trace.create test_tool in
  let t' = Bap_trace.create test_tool in
  let id,id' = Bap_trace.(id t, id t') in
  let not_eql = not (Bap_trace.Id.equal id id') in
  assert_bool "is mustn't be equal, failed" not_eql

let trace_tool ctxt = 
  let t = Bap_trace.create test_tool in
  let tool = Bap_trace.tool t in
  assert_equal ~ctxt tool test_tool

let set_attr t attr a ctxt = 
  let t = Bap_trace.set_attr t attr a in
  match Bap_trace.get_attr t attr with
  | None -> assert_failure "get attribute failed"
  | Some a' -> assert_equal ~ctxt a a'

let has_attr t attr a ctxt = 
  let t = Bap_trace.set_attr t attr a in
  let has = Bap_trace.has_attr t attr in
  assert_bool "attribute is absent" has

let set_meta t ctxt =
  let open Value.Dict in 
  let t = Bap_trace.set_meta t test_meta in
  let meta = Bap_trace.meta t in 
  assert_equal ~ctxt ~cmp:dict_eql meta test_meta

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

let find_all_matching ctxt = 
  let t = empty in
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
     Seq.length s' = 2)

let fold_matching ctxt = 
  let t = empty in
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
    (List.for_all s ~f:is_memory && List.length s = 2)

let supports ctxt = 
  let m = Bap_trace.supports empty memory_load in
  let m' = Bap_trace.supports empty memory_store in
  let r = not (Bap_trace.supports empty register_read) in
  assert_bool "supports failed" (m && m' && r)

let contains t ctxt = 
  let t = Bap_trace.add_event t pc_update Word.b1 in
  match Bap_trace.contains t pc_update with
  | Some r -> assert_bool "contains failed" r
  | None -> assert_failure "contains failed"

let append ctxt =
  let t = empty in
  let c = Value.create in
  let t = Bap_trace.add_event t register_read reg in
  let t = Bap_trace.add_event t register_write reg in
  let evs = [c register_read reg; c register_write reg] in
  let expected = Seq.of_list (evs @ test_events) in
  let t = Bap_trace.append t (Seq.of_list test_events) in
  let events = Bap_trace.events t in
  assert_equal ~ctxt ~cmp:seq_eql events expected

let next () = 
    let a = ref test_events in
    fun () -> match !a with
      | [] -> None
      | hd::tl -> a := tl; Some hd 

let unfold ctxt = 
  let t = Bap_trace.unfold test_tool ~f:(next ()) ~init:() in
  let evs = Bap_trace.events t in
  let evs' = Seq.of_list test_events in
  assert_equal ~ctxt ~cmp:seq_eql evs evs'

let check_calls ctxt = 
  let t = Bap_trace.unfold test_tool ~f:(next ()) ~init:() in
  let evs = Bap_trace.events t in
  let _ = Bap_trace.find t register_read in
  let evs' = Bap_trace.events t in
  assert_equal ~ctxt ~cmp:seq_eql evs evs'

let memoize ctxt = 
  let a = ref 0 in
  let next = 
    let evs = ref test_events in
    fun () -> match !evs with
      | [] -> None
      | hd::tl -> 
        a := !a + 1; evs := tl; Some hd in
  let t = Bap_trace.unfold test_tool ~f:next ~init:() in
  assert_equal ~ctxt !a 0;
  let _ = Bap_trace.memoize t in
  assert_equal ~ctxt !a (List.length test_events)

let suite =  
  "Trace" >:::
  [
    "save_and_load"     >:: save_and_load;
    "id"                >:: id;
    "tool"              >:: trace_tool;
    "set_attr"          >:: set_attr empty binary bin;
    "has_attr"          >:: has_attr empty binary bin;
    "set_meta"          >:: set_meta empty;
    "add_event"         >:: add_event empty memory_load move;
    "find"              >:: find empty memory_load move;
    "find_all"          >:: find_all empty 5 memory_load move;
    "find_all_matching" >:: find_all_matching ;
    "fold_matching"     >:: fold_matching;
    "supports"          >:: supports;
    "contains"          >:: contains empty;
    "append"            >:: append;
    "unfold"            >:: unfold;
    "check_calls"       >:: check_calls;
    "memoize"           >:: memoize;
  ]
