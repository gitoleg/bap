open Core_kernel.Std
open Bap_types.Std
open Bap_trace_event_types

module Move = struct
  include Move
  let pp pp_cell arr ppf t =
    Format.fprintf ppf "%a %s %a" pp_cell t.cell arr Word.pp t.data
end

module Load = struct
  type t = addr move with bin_io, compare, sexp
  let pp = Move.pp Addr.pp "=>"
end

module Store = struct
  type t = addr move with bin_io, compare, sexp
  let pp = Move.pp Addr.pp "<="
end

module Read = struct
  type t = var move with bin_io, compare, sexp
  let pp = Move.pp Var.pp "=>"
end

module Write = struct
  type t = var move with bin_io, compare, sexp
  let pp = Move.pp Var.pp "<="
end

module Chunk = struct
  include Chunk
  let pp ppf t = failwith "unimplemented"
end

module Syscall = struct
  include Syscall
  let pp ppf s = failwith "unimplemented"
end

module Exn = struct
  include Exn
  let pp ppf s = failwith "unimplemented"
end

module Call = struct
  include Call
  let pp ppf s = failwith "unimplemented"
end

module Return = struct
  include Return
  let pp ppf s = failwith "unimplemented"
end

module Modload = struct
  include Modload
  let pp fmt t =
    Format.fprintf fmt "%s: %a - %a" t.name Addr.pp t.low Addr.pp t.high
end

let memory_load =
  Value.Tag.register (module Load)
    ~name:"memory_load"
    ~uuid:"9546a981-de85-4e5c-8d59-73a15bf5c7bd"

let memory_store =
  Value.Tag.register (module Store)
    ~name:"memory_store"
    ~uuid:"d5995d83-76be-410d-94a9-b0cfcb91f2de"

let register_read =
  Value.Tag.register (module Read)
    ~name:"register_read"
    ~uuid:"ded5dc91-dafc-4316-9c6c-4dad4e40a273"

let register_write =
  Value.Tag.register (module Write)
    ~name:"register_write"
    ~uuid:"395f5f37-5aed-4bd2-a51f-1c7216b5cd7c"

let timestamp =
  Value.Tag.register (module Int64)
    ~name:"timestamp"
    ~uuid:"0feea5c2-b471-48e4-a10f-c7e18cbf21a9"

let pc_update =
  Value.Tag.register (module Addr)
    ~name:"pc_update"
    ~uuid:"98ea397e-d726-43be-9ec5-bf226d67578f"

let code_exec =
  Value.Tag.register (module Chunk)
    ~name:"code_exec"
    ~uuid:"b8b3af3a-d1aa-4bf0-a36f-4ea6d0dd2bbf"

let context_switch =
  Value.Tag.register (module Int)
    ~name:"context_switch"
    ~uuid:"7f1d322a-d2cc-4e42-8e7a-081080751268"

let syscall =
  Value.Tag.register (module Syscall)
    ~name:"syscall"
    ~uuid:"6e0eec5c-2907-4c4c-b9b1-b879d2cbc69b"

let exn =
  Value.Tag.register (module Exn)
    ~name:"exn"
    ~uuid:"18ae62d6-aa66-429b-964c-e15b7913d57e"

let call =
  Value.Tag.register (module Call)
    ~name:"call"
    ~uuid:"fe9899fe-3b60-4d10-bb50-dbccfc4ee0da"

let return =
  Value.Tag.register (module Return)
    ~name:"return"
    ~uuid:"2cae388a-69cb-48a0-8355-d3f9d39ac8eb"

let modload =
  Value.Tag.register (module Modload)
    ~name:"modload"
    ~uuid:"7f842d03-6c9f-4745-af39-002f468f7fc8"
