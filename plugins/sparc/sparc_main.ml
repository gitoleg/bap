open Core_kernel.Std
open Bap.Std
open Sparc.Std

include Self()

let () = Config.manpage [
    `S "DESCRIPTION";
    `P "Provides lifter for Sparc architecture.";
  ]

let () =
  Sparc_branch.init ();
  Sparc_add.init ();
  Sparc_logic.init ();
  Sparc_misc.init ();
  Sparc_sub.init ();
  Sparc_load.init ();
  Sparc_store.init ()


module Make(T : Target) : Target = struct
  open Format
  include T

  let pp_insn ppf (mem,insn) =
    fprintf ppf "%a: %s"
      Addr.pp_hex (Memory.min_addr mem)
      (Disasm_expert.Basic.Insn.asm insn)

  let lift mem insn =
    match lift mem insn with
    | Error err as failed ->
      warning "can't lift instruction %a - %a"
        pp_insn (mem,insn) Error.pp err;
      failed
    | Ok bil as ok -> match Type.check bil with
      | Ok () -> ok
      | Error te ->
        warning "BIL doesn't type check %a - %a"
          pp_insn (mem,insn) Type.Error.pp te;
        Error (Error.of_string "type error")
end

module Sparc64 = Make(T64)

let () =
  Config.when_ready (fun _ ->
      register_target `sparcv9 (module Sparc64);
      Sparc_abi.setup ()
    )
