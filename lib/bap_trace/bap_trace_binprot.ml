
module Or_error = Core_kernel.Std.Or_error
open Or_error

open Bap_types.Std

(** TODO: remove this  *)
(* Writer.write_bin_prot writer bin_writer_t meta *)
module type S = module type of Bap_trace

module Proto = struct
  open Core_kernel.Std
  open Bin_prot

  let name = "bap_trace.binprot"
  
  let read chan reader = 
    let s = input_line chan in
    let s = Bigstring.of_string s in
    let pos_ref = ref 0 in
    let n = (Read.bin_read_nat0 s ~pos_ref :> int) in
    let () = Printf.printf "read str len %d\n" n in
    reader.Type_class.read s ~pos_ref 
      
  let write chan writer value =
    let s = Writer.to_string writer value in
    let () = Printf.printf "write str %s\n" s in
    let () = Printf.printf "write str len %d\n" (String.length s) in
    output_string chan s;
    output_string chan "\n"

  let read_tool  chan = read chan Bap_trace.bin_reader_tool
  let read_event chan = read chan Bap_trace.bin_reader_event
  let read_meta  chan = read chan Value.Dict.bin_reader_t
  let write_tool chan tool = write chan Bap_trace.bin_writer_tool tool
  let write_meta chan meta = write chan Value.Dict.bin_writer_t meta
  let write_event  chan ev = write chan Bap_trace.bin_writer_event ev
  let probe uri = Uri.scheme uri = Some "file" 
  let supports: 'a tag -> bool = fun _ -> true
  
end

let make_in_channel uri = open_in (Uri.path uri)
let make_out_channel uri = open_out (Uri.path uri)

(** TODO: add error handling here  *)
let write: Uri.t -> Bap_trace.t -> unit Or_error.t 
  = fun uri t ->
    let ch = make_out_channel uri in
    let evs = Bap_trace.events t in
    let () = Proto.write_tool ch (Bap_trace.tool t) in
    let () = Proto.write_meta ch (Bap_trace.meta t) in
    Seq.iter evs ~f:(fun ev -> Proto.write_event ch ev);
    flush ch;
    close_out ch;
    Ok ()

let read: Uri.t -> Bap_trace.id -> Bap_trace.reader 
  = fun uri t ->
    let ch = make_in_channel uri in
    let next () = 
      try 
        Some (Proto.read_event ch)
      with End_of_file -> 
        close_in ch; None in      
      let tool = Proto.read_tool ch in
      let meta = Proto.read_meta ch in
    Bap_trace.Reader.({tool; meta; next;})

let register () =
  let open Bap_trace in
  let proto = register_proto (module Proto : P) in
  register_reader proto read;
  register_writer proto write

