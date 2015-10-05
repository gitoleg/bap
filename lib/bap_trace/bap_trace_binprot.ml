
open Core_kernel.Std
open Or_error
open Bin_prot

open Bap_types.Std

module Proto = struct
  open Core_kernel.Std
  open Bin_prot

  let name = "bap_trace.binprot"
  let probe uri = Uri.scheme uri = Some "file" 
  let supports: 'a tag -> bool = fun _ -> true
 
  let read chan reader = 
    let read_from_channel len = 
      let buf = String.create len in
      match In_channel.really_input chan ~buf ~pos:0 ~len with
      | None -> None
      | Some () -> Some (Bigstring.of_string buf) in
    let read_from_buf reader buf = 
      let pos_ref = ref 0 in
      reader buf ~pos_ref in
    let hdr_len = Bin_prot.Utils.size_header_length in
    match read_from_channel hdr_len with
    | None -> None
    | Some buf -> 
      let data_len = read_from_buf Utils.bin_read_size_header buf in
      match read_from_channel data_len with
      | None -> None
      | Some buf -> 
      let r = reader.Bin_prot.Type_class.read  in
      Some (read_from_buf r buf)

  (** TODO: add error handling here  *)
  let write chan writer value =
    let data_len = writer.Bin_prot.Type_class.size value in
    let total_len = data_len + Bin_prot.Utils.size_header_length in
    let buf = Bigstring.create total_len in
    let pos = Bin_prot.Utils.bin_write_size_header buf ~pos:0 data_len in
    let pos' = writer.Bin_prot.Type_class.write buf ~pos value in
    let s = Bigstring.to_string buf in
    output_string chan s

  let read_tool  chan = read chan Bap_trace.bin_reader_tool
  let read_meta  chan = read chan Value.Dict.bin_reader_t
  let read_event chan = read chan Bap_trace.bin_reader_event
  let write_tool chan tool = write chan Bap_trace.bin_writer_tool tool
  let write_meta chan meta = write chan Value.Dict.bin_writer_t meta
  let write_event chan ev = write chan Bap_trace.bin_writer_event ev
  
end

let make_in_channel uri = In_channel.create (Uri.path uri)
let make_out_channel uri = Out_channel.create (Uri.path uri)

(** TODO: add error handling here  *)
let write: Uri.t -> Bap_trace.t -> unit Or_error.t 
  = fun uri t ->
    let ch = make_out_channel uri in
    let () = Proto.write_tool ch (Bap_trace.tool t) in
    let () = Proto.write_meta ch (Bap_trace.meta t) in
    let evs = Bap_trace.events t in
    Seq.iter evs ~f:(fun ev -> Proto.write_event ch ev);
    Out_channel.flush ch;
    Out_channel.close ch;
    Ok ()

let next ch = fun () ->
  try 
    Proto.read_event ch
  with End_of_file -> In_channel.close ch; None 
      
(** TODO: add error handling here  *)
let read: Uri.t -> Bap_trace.id -> Bap_trace.reader 
  = fun uri t ->
    let ch = make_in_channel uri in    
    let tool = Option.value_exn (Proto.read_tool ch) in
    let meta = Option.value_exn (Proto.read_meta ch) in
    Bap_trace.Reader.({tool; meta; next = next ch;})

let register () =
  let open Bap_trace in
  let proto = register_proto (module Proto : P) in
  register_reader proto read;
  register_writer proto write

