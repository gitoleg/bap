
open Core_kernel.Std
open Result
open Bin_prot

open Bap_types.Std

module Proto = struct

  let name = "trace.binprot"
  let probe uri = Uri.scheme uri = Some "file" 
  let supports: 'a tag -> bool = fun _ -> true
 
  let read_from_channel chan len = 
    let buf = String.create len in
    match In_channel.really_input chan ~buf ~pos:0 ~len with
    | None -> None
    | Some () -> Some (Bigstring.of_string buf) 

  let read_from_buf reader buf = 
    let pos_ref = ref 0 in
    reader buf ~pos_ref 

  let read reader chan = 
    let open Option in
    read_from_channel chan Utils.size_header_length >>=
    fun buf ->
    read_from_buf Utils.bin_read_size_header buf |>
    read_from_channel chan >>=
    fun buf' -> Some (read_from_buf reader.Type_class.read buf')

  let write writer value chan =
    let data_len = writer.Type_class.size value in
    let total = data_len + Utils.size_header_length in
    let buf = Bigstring.create total in
    let pos = Utils.bin_write_size_header buf ~pos:0 data_len in
    let _ = writer.Type_class.write buf ~pos value in
    Out_channel.output_string chan (Bigstring.to_string buf)

  let read_tool   = read  Bap_trace.bin_reader_tool
  let read_meta   = read  Value.Dict.bin_reader_t 
  let read_event  = read  Bap_trace.bin_reader_event
  let write_tool  = write Bap_trace.bin_writer_tool
  let write_meta  = write Value.Dict.bin_writer_t
  let write_event = write Bap_trace.bin_writer_event 

end

let make_in_channel path = 
  try 
    let () = Unix.(access path [R_OK]) in
    Ok (In_channel.create path)
  with Unix.Unix_error (er,_,_) -> Error (`System_error er)

let make_out_channel path =
  try
    let fd = Unix.(openfile path [O_WRONLY; O_TRUNC; O_CREAT] 0o666) in
    Ok (Unix.out_channel_of_descr fd)
  with Unix.Unix_error (er,_,_) -> Error (`System_error er)

let write uri trace =
  make_out_channel (Uri.path uri) >>=
  fun ch ->
  let () = Proto.write_tool (Bap_trace.tool trace) ch in
  let () = Proto.write_meta (Bap_trace.meta trace) ch in
  let evs = Bap_trace.events trace in
  Seq.iter evs ~f:(fun ev -> Proto.write_event ev ch);
  Ok (Out_channel.close ch)

let next_event ch = 
  try 
    match Proto.read_event ch with
    | None -> None
    | Some ev -> Some (Ok ev)
  with 
  | End_of_file -> In_channel.close ch; None 
  | exn -> 
    let err = Error.of_info (Info.of_exn exn) in
    Some (Error err)

let read uri id =
  make_in_channel (Uri.path uri) >>=
  fun ch ->
  try
    let next () = next_event ch in
    let tool = Option.value_exn (Proto.read_tool ch) in
    let meta = Option.value_exn (Proto.read_meta ch) in
    Ok (Bap_trace.Reader.({tool; meta; next;}))
  with 
  | exn -> 
    let err = Error.of_info (Info.of_exn exn) in
    Error (`Protocol_error err)
    
let register () =
  let open Bap_trace in
  let proto = register_proto (module Proto : P) in
  register_reader proto read;
  register_writer proto write
