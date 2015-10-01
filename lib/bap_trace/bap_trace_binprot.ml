
module Or_error = Core_kernel.Std.Or_error
open Or_error

open Bap_types.Std

module Proto = struct
  open Core_kernel.Std
  open Bin_prot

  let probe uri = 
    let () = Printf.printf "path: %s\n" (Uri.to_string uri) in
    Uri.scheme uri = Some "file" 

  let supports: 'a tag -> bool = fun _ -> true

  let read chan reader = 
    let s = input_line chan in
    let pos_ref = ref 0 in
    let buf = Bigstring.of_string s in
    reader.Type_class.read buf ~pos_ref 
      
  let write chan writer value =
    let s = Writer.to_string writer value in
    output_string chan s

  let read_tool  chan = read chan Bap_trace.bin_reader_tool
  let read_event chan = read chan Bap_trace.bin_reader_event
  let read_meta  chan = read chan Value.Dict.bin_reader_t
  let write_tool chan tool = write chan Bap_trace.bin_writer_tool tool
  let write_meta chan meta = write chan Value.Dict.bin_writer_t meta
  let write_event chan ev = write chan Bap_trace.bin_writer_event ev
   

end

module type B = module type of Bap_trace

let make_in_channel uri = 
  open_in_gen [Open_rdonly;] 0o444 (Uri.path uri)

let make_out_channel uri = 
  open_out_gen [Open_wronly;] 0o444 (Uri.path uri)

let write: Uri.t -> Bap_trace.t -> unit Or_error.t 
  = fun uri t ->
    let ch = make_out_channel uri in
    let evs = Bap_trace.events t in
    let () = Proto.write_tool ch (Bap_trace.tool t) in
    let () = Proto.write_meta ch (Bap_trace.meta t) in
    Seq.iter evs ~f:(fun ev -> Proto.write_event ch ev);
    Ok (close_out ch)

let read: Uri.t -> Bap_trace.id -> Bap_trace.reader 
  = fun uri t ->
    let ch = make_in_channel uri in
    let tool = Proto.read_tool ch in
    let meta = Proto.read_meta ch in
    let next () = Proto.read_event ch in
    Bap_trace.Reader.({tool; meta; next;})

let () = Printf.printf "registerING bin proto \n" 
let proto = 
  Bap_trace.register_proto ~name:"trace.bin_proto" ~probe:Proto.probe
    ~supports:Proto.supports
    
let () = Bap_trace.register_reader proto read
let () = Bap_trace.register_writer proto write

