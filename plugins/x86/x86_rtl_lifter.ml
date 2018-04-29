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
  pc          : rhs exp;
  next        : rhs exp;
}

module Common = struct
  let e = Exp.of_var

  let cf = e cf
  let oF = e oF
  let pf = e pf
  let af = e af
  let zf = e zf
  let sf = e sf
end

module type Mode = sig
  val addr_size : size
  include ModeVars
end

module Make(R : Mode) = struct
  open R

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


  let model = if Size.equal addr_size `r32 then model
    else
      let low x = RTL.low word (Exp.of_var x) in
      List.fold ~init:model
        ~f:(fun m (n,r) -> Reg_model.Exp.add Cls.gpr (`Name n) (low r) m)
        [ "EBP", rbp; "ESP", rsp; "ESI", rsi; "EDI", rdi;
          "EIP", rip; "EAX", rax; "EBX", rbx;  "ECX", rcx;
          "EDX", rdx; ]

  let load addr size = Mem_model.load R.mem LittleEndian addr size
  let store addr data size =
    Mem_model.store R.mem LittleEndian addr data size

  let addr_bits = Size.in_bits addr_size

  let reg_or_nil : (op -> rhs exp) ec =
    let find op =
      let reg = match op with
        | Op.Reg reg ->
          let name = `Name (Reg_op.name reg) in
          Reg_model.Exp.find model name
        | _ -> None in
      match reg with
      | Some x -> Exp.as_rhs x
      | None -> Exp.of_word (Word.zero addr_bits) in
    Ec.create find


  let make_cpu mem =
    let pc = Memory.min_addr mem in
    let next = Addr.(pc + of_int ~width:addr_bits (Memory.length mem)) in {
      reg = Reg_model.ec model;
      reg_or_nil;
      load;
      store;
      word_width  = word;
      word_width' = Exp.of_word (Word.of_int ~width:addr_bits addr_bits);
      sp = Exp.of_var rsp;
      rax = Exp.of_var rax;
      pc = Exp.signed @@ Exp.of_word pc;
      next = Exp.signed @@ Exp.of_word next;
    }

end

include Lifter_model.Make(struct
    type t = cpu

    let update cpu _addr = cpu
  end)

module IA32R = struct

  module R32 = struct
    include R32
    let addr_size = `r32
  end

  module L = Make(R32)

  let fails = String.Table.create ()

  let lift mem insn =
    init @@ L.make_cpu mem;
    let r = lifter mem insn in
    if Result.is_ok r then r
    else
      let n = Insn.name @@ Insn.of_basic insn in
      Hashtbl.set fails n (Insn.asm @@ Insn.of_basic insn);
      r

  let pr () =
    Hashtbl.iteri ~f:(fun ~key:n ~data:asm -> printf "%s %s\n" n asm) fails

  let () = at_exit pr

  module CPU = IA32
end

module AMD64R = struct

  module R64 = struct
    include R64
    let addr_size = `r64
  end

  module L = Make(R64)
  let lift mem insn =
    init @@ L.make_cpu mem;
    lifter mem insn

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
  let base  = unsigned cpu.reg_or_nil ops.(0) in
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

(** 74 f6 *)
let je cpu ops =
  let x = signed imm ops.(0) in
  RTL.[
    when_ zf [
      jmp (cpu.next + x);
    ];
  ]

let () = register "JE_1" je

(* 0x83,0x45,0xc0,0x01 *)
let add32mi8 cpu ops =
  let base = unsigned cpu.reg ops.(0) in
  let scale = unsigned imm ops.(1) in
  let index = unsigned cpu.reg_or_nil ops.(2) in
  let disp = unsigned imm ops.(3) in
  let _xz = unsigned cpu.reg_or_nil ops.(4) in
  let imm = unsigned imm ops.(5) in
  let addr = unsigned var cpu.word_width in
  let tmp1 = unsigned var word in
  let tmp2 = unsigned var word in
  RTL.[
    addr := base + scale * index + disp;
    tmp1 := cpu.load addr word;
    tmp2 := imm;
    cpu.store addr (tmp1 + tmp2) word
  ]


let stub _cpu _ops = []

let () = register "ADD32mi8" add32mi8
let () = register "ADD32ri" stub
let () = register "ADD32ri8" stub
let () = register "ADD32rr" stub


(**

ADD32mi8 addl $0x1, -0x40(%ebp)
ADD32ri addl $0x1885, %ebx
ADD32ri8 addl $0x1, %eax
ADD32rr addl %edx, %eax
AND32ri8 andl $-0x10, %esp
CALL32m calll *-0xf8(%ebx,%edi,4)
CALLpcrel32 calll -0x41b
CMP32rm cmpl -0x34(%ebp), %eax
CMP32rr cmpl %esi, %edi
DIV32r divl %esi
HLT hlt
IMUL32rri8 imull $0x10, %eax, %eax
JL_1 jl -0x1d
JL_4 jl -0x9b
JMP_1 jmp 0x11
JMP_4 jmp 0x8f
JNE_1 jne -0x21
LEA32r leal -0xf8(%ebx), %eax
LEAVE leave
NOOP nop
POP32r popl %ebp
PUSH32rmm pushl -0x4(%ecx)
RETL retl
SAR32ri sarl $0x2, %esi
SHL32ri shll $0x2, %eax
SHR32ri shrl $0x2, %eax
SUB32ri8 subl $0x1c, %esp
SUB32rr subl %eax, %esi
XOR32rr xorl %edi, %edi
*)
