
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

  let read_value chan reader size =
    match read_from_channel chan size with
    | None -> None 
    | Some buf -> 
      try 
        Some (Ok (read_from_buf reader buf))
      with exn -> Some (Error (Error.of_exn exn))

  let read_size ch =
    read_value ch Utils.bin_read_size_header Utils.size_header_length

  let read reader ch = match read_size ch with
    | None -> None
    | Some (Error er) as err -> err 
    | Some (Ok size) -> read_value ch reader.Type_class.read size

  let write writer value chan =
    let data_len = writer.Type_class.size value in
    let total_len = data_len + Utils.size_header_length in
    let buf = Bigstring.create total_len in
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

let next_event ch = match Proto.read_event ch with
  | None -> In_channel.close ch; None
  | Some ev as res -> res

let error_msg () = Error.of_string "trace has damaged header"

let err_of_opt read ch = match read ch with
  | Some res -> res
  | None -> 
    In_channel.close ch; 
    Error (error_msg ())

let make_reader ch =
  let next () = next_event ch in
  err_of_opt Proto.read_tool ch >>=
  fun tool -> err_of_opt Proto.read_meta ch >>=
  fun meta -> Ok (Bap_trace.Reader.({tool; meta; next;}))

let read uri id =
  make_in_channel (Uri.path uri) >>=
  fun ch -> match make_reader ch with 
  | Ok r as res -> res
  | Error er -> Error (`Protocol_error er)
    
let register () =
  let open Bap_trace in
  let proto = register_proto (module Proto : P) in
  register_reader proto read;
  register_writer proto write
