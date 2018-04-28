open Core_kernel.Std
open Bap.Std
open Bap_rtl.Std
open X86_env
open X86_cpu

module Reg_op = Reg

type cpu = {
  reg         : (op -> lhs exp) ec;
  reg_or_nil  : (op -> rhs exp) ec;
  load        : 'a. 'a exp -> bitwidth -> rhs exp;
  store       : 'a 'b. 'a exp -> 'b exp -> bitwidth -> rtl;
  word_width  : bitwidth;
  word_width' : rhs exp;
  sp          : lhs exp;
  rax         : lhs exp;
}

module Common = struct
  let cf = Exp.of_var cf
  let oF = Exp.of_var oF
  let pf = Exp.of_var pf
  let af = Exp.of_var af
  let zf = Exp.of_var zf
  let sf = Exp.of_var sf
end

module R32 = struct
  open R32

  open RTL
  open Bitwidth

  let model = Reg_model.empty

  let al = RTL.low byte (Exp.of_var rax)

  let add cls model reg = Reg_model.add cls reg model

  let model =
    let seg = Cls.of_string "seg" in
    let model = List.fold ~init:model
        ~f:(add Cls.gpr)
        [rbp;rsp;rsi;rdi;rip;rax;rbx;rcx;rdx] in
    let model = Reg_model.Exp.add Cls.gpr (`Name "AL") al model in
    let model = List.fold ~f:(add seg) ~init:model [fs_base; gs_base] in
    Array.fold ~init:model ~f:(add Cls.vector) ymms

  let load addr size = Mem_model.load R32.mem LittleEndian addr size
  let store addr data size = Mem_model.store R32.mem LittleEndian addr data size

  let reg_or_nil : (op -> rhs exp) ec =
    let find op =
      let reg = match op with
        | Op.Reg reg ->
          let name = `Name (Reg_op.name reg) in
          Reg_model.Exp.find model name
        | _ -> None in
      match reg with
      | Some x -> Exp.as_rhs x
      | None -> Exp.of_word (Word.zero 32) in
    Ec.create find

  let cpu = {
    reg = Reg_model.ec model;
    reg_or_nil;
    load;
    store;
    word_width  = word;
    word_width' = Exp.of_word (Word.of_int ~width:32 32);
    sp = Exp.of_var rsp;
    rax = Exp.of_var rax;
  }

  include Lifter_model.Make(struct
    type t = cpu

    let update cpu _addr = cpu
  end)

  let () = init cpu

end

module IA32R = struct
  let lift = R32.lifter

  module CPU = IA32
end

(** TODO: need to do a lifter  *)
module AMD64R = struct
  let lift = R32.lifter

  module CPU = AMD64
end


open Ec
open Bitwidth
open Common
open R32

let push32_r cpu ops =
  let src = unsigned cpu.reg ops.(0) in
  let tmp = unsigned var cpu.word_width in
  let bytes = unsigned const word 8 in
  RTL.[
    tmp := src;
    cpu.sp := cpu.sp - cpu.word_width' / bytes;
    cpu.store cpu.sp tmp word;
  ]

let push32_i cpu ops =
  let src = unsigned imm ops.(0) in
  let tmp = unsigned var cpu.word_width in
  let bytes = unsigned const word 8 in
  RTL.[
    tmp := src;
    cpu.sp := cpu.sp - cpu.word_width' / bytes;
    cpu.store cpu.sp tmp word;
  ]

let push32_rmm cpu ops =
  let base  = unsigned R32.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg ops.(2) in
  let disp  = unsigned imm ops.(3) in
  let tmp = unsigned var cpu.word_width in
  let bytes = unsigned const word 8 in
  RTL.[
    tmp := cpu.load (base + scale * index + disp) word;
    cpu.sp := cpu.sp - cpu.word_width' / bytes;
    cpu.store cpu.sp tmp word;
  ]

let () = register "PUSH32r" push32_r
let () = register "PUSHi32" push32_i
let () = register "PUSH32rmm" push32_rmm


let mov32_rr cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned cpu.reg ops.(1) in
  RTL.[ dst := src ]

let mov32_rm cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let base = unsigned cpu.reg_or_nil ops.(1) in
  let index = unsigned imm ops.(2) in
  let scale = unsigned cpu.reg_or_nil ops.(3) in
  let disp  = unsigned imm ops.(4) in
  RTL.[
    dst := cpu.load (base + scale * index + disp) word;
  ]

let mov32_ri cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned imm ops.(1) in
  RTL.[ dst := src ]

let mov32_mi cpu ops =
  let base = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp  = unsigned imm ops.(3) in
  let x = unsigned imm ops.(5) in
  RTL.[
    cpu.store (base + scale * index + disp) x word ;
  ]

let mov32_mr cpu ops =
  let base = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp  = unsigned imm ops.(3) in
  let x = unsigned cpu.reg ops.(5) in
  RTL.[
    cpu.store (base + scale * index + disp) x word ;
  ]

let mov8_mi cpu ops =
  let base = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp  = unsigned imm ops.(3) in
  let x = unsigned imm ops.(5) in
  RTL.[
    cpu.store (base + scale * index + disp) x byte;
  ]

let mov32_ao_32 cpu ops =
  let src = unsigned imm ops.(0) in
  RTL.[
    cpu.rax := cpu.load src word;
  ]

let movzx32rm8 cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let base = unsigned cpu.reg_or_nil ops.(1) in
  let index = unsigned imm ops.(2) in
  let scale = unsigned cpu.reg_or_nil ops.(3) in
  let disp  = unsigned imm ops.(4) in
  RTL.[
    dst := cpu.load (base + scale * index + disp) byte;
  ]

let movsx32rr8 cpu ops =
  let dst = signed cpu.reg ops.(0) in
  let src = signed cpu.reg ops.(1) in
  RTL.[
    dst := src;
  ]

let () = register "MOV32rr" mov32_rr
let () = register "MOV32rm" mov32_rm
let () = register "MOV32ri" mov32_ri
let () = register "MOV32mi" mov32_mi
let () = register "MOV32mr" mov32_mr
let () = register "MOV8mi" mov8_mi
let () = register "MOV32ao32" mov32_ao_32
let () = register "MOVZX32rm8" movzx32rm8
let () = register "MOVSX32rr8" movsx32rr8


let test32_rr cpu ops =
  let op1 = unsigned cpu.reg ops.(0) in
  let op2 = unsigned cpu.reg ops.(1) in
  let tmp = unsigned var word in
  RTL.[
    tmp := op1 land op2;
    oF := zero;
    cf := zero;
    pf := zero; (** TODO: parity flag need to be calculated *)
    sf := msb tmp;
    zf := tmp = zero;
    (** TODO: af flag is set with bil unknown - do we really need it ?  *)
  ]

let test8_rr cpu ops =
  let op1 = unsigned cpu.reg ops.(0) in
  let op2 = unsigned cpu.reg ops.(1) in
  let tmp = unsigned var byte in
  RTL.[
    tmp := op1 land op2;
    oF := zero;
    cf := zero;
    pf := zero; (** TODO: parity flag need to be calculated *)
    sf := msb tmp;
    zf := tmp = zero;
    (** TODO: af flag is set with bil unknown - do we really need it ?  *)
  ]


let () = register "TEST32rr" test32_rr
let () = register "TEST8rr" test8_rr

let je _cpu ops =
  let x = unsigned imm ops.(0) in
  RTL.[
    when_ zf [
      jmp x;
    ];
  ]

let () = register "JE_1" je



(**


    je
    call
    add
    pop
    lea
    ret
    sub
    xor
    and
    hlt
    xchg
    movl
    leave
    sar
    shl
    shr
    cmpb
    movb
    repz
    jmp
    nop
    movzbl
    movsbl
    imul
*)
