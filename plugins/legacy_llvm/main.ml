open Core_kernel.Std
open Bap.Std
include Self()

let run () =
  match Bap_llvm_loader.init () with
  | Ok () -> ()
  | Error er -> eprintf "%s\n" (Error.to_string_hum er)

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Provides a legacy llvm loader"
    ] in
  let enable =
    Config.flag ~doc:"enable llvm legacy loader" "enable" in
  Config.when_ready (fun {Config.get=(!)} ->
      if !enable then run ())
