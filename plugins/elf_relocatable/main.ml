open Core_kernel.Std
open Bap.Std

include Self()

let () = Rel_brancher.init ()

let main entry = ()

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "This plugin provides a brancher for elf relocatable files";
    ] in
  let entry =
    let doc = "Replaces a entry point in relocatable files" in
    Config.(param int ~default:0x401800 "entry" ~doc) in
  Config.when_ready (fun {Config.get=(!)} -> main !entry)
