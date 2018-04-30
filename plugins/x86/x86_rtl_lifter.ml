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
  rdx         : lhs exp;
  rbp         : lhs exp;
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
      rdx = Exp.of_var rdx;
      rbp = Exp.of_var rbp;
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

  let print_insn insn =
    let insn = Insn.of_basic insn in
    printf "asm : %s\n" (Insn.asm insn)


  let lift mem insn =
    printf "rtl lifter\n";
    print_insn insn;
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
  let scale = unsigned cpu.reg_or_nil ops.(2) in
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

(* 0xff,0x94,0xbb,0x08,0xff,0xff,0xff *)
let call32m cpu ops =
  let base  = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp = unsigned imm ops.(3) in
  let _xz = unsigned cpu.reg_or_nil ops.(4) in
  let tmp = unsigned var cpu.word_width in
  let eight = unsigned const word 8 in
  RTL.[
    tmp := cpu.load (base + scale * index + disp) cpu.word_width;
    cpu.sp := cpu.sp - cpu.word_width' / eight;
    cpu.store cpu.next cpu.sp cpu.word_width;
    jmp tmp;
  ]
(* 75 0a *)
let jne cpu ops =
  let disp = signed imm ops.(0) in
  RTL.[
    when_ (zf = zero) [
      jmp (cpu.next + disp);
    ]
  ]

(* 0x7c,A *)
let jl cpu ops =
  let imm = signed imm ops.(0) in
  RTL.[
    when_ (sf land oF) [
      jmp (cpu.next + imm);
    ]
  ]

(* eb,0a  *)
let jmp _cpu ops =
  let dst = unsigned imm ops.(0) in
  RTL.[jmp dst]

(* e8 0a 0a 0a 0a *)
let call_pcrel32 cpu ops =
  let disp = signed imm ops.(0) in
  let eight = unsigned const word 8 in
  RTL.[
    cpu.sp := cpu.sp - cpu.word_width'/eight;
    cpu.store cpu.sp cpu.next cpu.word_width;
    jmp (cpu.next + disp)
  ]

(* c9  *)
let leave cpu _ops =
  let eight = unsigned const word 8 in
  RTL.[
    cpu.sp := cpu.rbp;
    cpu.rbp := cpu.load cpu.sp cpu.word_width;
    cpu.sp := cpu.sp + cpu.word_width'/eight;
  ]

(* c3 *)
let retl cpu _ops =
  let eight = unsigned const word 8 in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := cpu.load cpu.sp cpu.word_width;
    cpu.sp := cpu.sp + cpu.word_width' / eight;
    jmp tmp;
  ]


let () = register "CALLpcrel32" call_pcrel32
let () = register "JNE_1" jne
let () = register "JL_1" jl
let () = register "JL_4" jl
let () = register "JMP_1" jmp
let () = register "JMP_4" jmp
let () = register "JE_1" je
let () = register "CALL32m" call32m
let () = register "LEAVE" leave
let () = register "RETL" retl

(* 0x83,0x45,0xc0,0x01 *)
let add32mi8 cpu ops =
  let base = unsigned cpu.reg ops.(0) in
  let scale = unsigned imm ops.(1) in
  let index = unsigned cpu.reg_or_nil ops.(2) in
  let disp = unsigned imm ops.(3) in
  let _xz = unsigned cpu.reg_or_nil ops.(4) in
  let imm = unsigned fixed_imm byte ops.(5) in
  let addr = unsigned var cpu.word_width in
  let tmp1 = signed var word in
  let tmp2 = signed var word in
  let resl = unsigned var word in
  RTL.[
    addr := base + scale * index + disp;
    tmp1 := cpu.load addr word;
    tmp2 := imm;
    resl := tmp1 + tmp2;
    cpu.store addr resl word;
    zf := resl = zero;
    sf := msb resl;
    cf := resl < tmp1;
    oF := (msb tmp1 = msb tmp2) land (msb tmp1 lxor msb resl);
  ]

(* 0x81,0xc3,0x21,0x04,0x00,0x00 *)
let add32ri cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned cpu.reg ops.(1) in
  let imm = unsigned imm ops.(2) in
  let tmp = signed var word in
  RTL.[
    tmp := src;
    dst := src + imm;
    zf := dst = zero;
    sf := msb dst;
    cf := dst < tmp;
    oF := (msb tmp = msb imm) land (msb tmp lxor msb dst);
  ]

(* 0x83,0xc3,0x42 *)
let add32ri8 cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned cpu.reg ops.(1) in
  let imm = unsigned fixed_imm byte ops.(2) in
  let tmp1 = unsigned var word in
  let tmp2 = unsigned var word in
  RTL.[
    tmp1 := src;
    tmp2 := imm;
    dst := src + imm;
    zf := dst = zero;
    sf := msb dst;
    cf := dst < tmp1;
    oF := (msb tmp1 = msb tmp2) land (msb tmp1 lxor msb dst);
  ]

(* 0x01,0xc3 *)
let add32rr cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src1 = unsigned cpu.reg ops.(1) in
  let src2 = unsigned cpu.reg ops.(2) in
  let tmp1 = signed var word in
  let tmp2 = signed var word in
  RTL.[
    tmp1 := src1;
    tmp2 := src2;
    dst := src1 + src2;
    zf := dst = zero;
    sf := msb dst;
    cf := dst < tmp1;
    oF := (msb tmp1 = msb tmp2) land (msb tmp1 lxor msb dst);
  ]

let stub _cpu _ops = []

let () = register "ADD32mi8" add32mi8
let () = register "ADD32ri"  add32ri
let () = register "ADD32ri8" add32ri8
let () = register "ADD32rr"  add32rr

(* 83 e4 f0 *)
let and32ri8 cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned cpu.reg ops.(1) in
  let imm = unsigned fixed_imm byte ops.(2) in
  let tmp = signed var word in
  RTL.[
    tmp := imm;
    dst := src land tmp;
    sf := msb dst;
    zf := dst = zero;
    oF := zero;
    cf := zero;
  ]

(* 0x31,0xf8 *)
let xor32rr cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src1 = unsigned cpu.reg ops.(1) in
  let src2 = unsigned cpu.reg ops.(2) in
  RTL.[
    dst := src1 lxor src2;
    oF := zero;
    cf := zero;
    zf := dst = zero;
    sf := msb dst;
  ]

let () = register "AND32ri8" and32ri8
let () = register "XOR32rr" xor32rr

(* 0x3b,0x45,0xcc  *)
let cmp32rm cpu ops =
  let src1 = unsigned cpu.reg ops.(0) in
  let base  = unsigned cpu.reg_or_nil ops.(1) in
  let index = unsigned imm ops.(2) in
  let scale = unsigned cpu.reg_or_nil ops.(3) in
  let disp = unsigned imm ops.(4) in
  let src2 = unsigned var cpu.word_width in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    src2 := cpu.load (base + scale * index + disp) cpu.word_width;
    tmp := src1 - src2;
    cf := src1 < src2;
    sf := msb tmp;
    zf := tmp = zero;
    oF := (src1 lxor src2) land (src1 lxor tmp);
  ]

(* 0x39,0xf7 *)
let cmp32rr cpu ops =
  let src1 = unsigned cpu.reg ops.(0) in
  let src2 = unsigned cpu.reg ops.(1) in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := src1 - src2;
    cf := src1 < src2;
    sf := msb tmp;
    zf := tmp = zero;
    oF := (src1 lxor src2) land (src1 lxor tmp);
  ]

let () = register "CMP32rm" cmp32rm
let () = register "CMP32rr" cmp32rr

(* f4 *)
let hlt _cpu _ops = []

(* 90 *)
let noop _cpu _ops = []

let () = register "HLT" hlt
let () = register "NOOP" noop

(* 5d *)
let pop32r cpu ops =
  let reg = unsigned cpu.reg ops.(0) in
  let eight = unsigned const cpu.word_width 8 in
  RTL.[
    reg := cpu.load cpu.sp cpu.word_width;
    cpu.sp := cpu.sp + cpu.word_width' / eight;
  ]

let () = register "POP32r" pop32r

(* f7 f6 *)
(* todo: some cpu exn here  *)
let div32r cpu ops =
  let reg = unsigned cpu.reg ops.(0) in
  let div = unsigned var word in
  let rem = unsigned var word in
  RTL.[
    div := (first cpu.rdx word ^ first cpu.rax word) / reg;
    rem := (first cpu.rdx word ^ first cpu.rax word) % reg;
    first cpu.rax word := div;
    first cpu.rdx word := rem;
  ]

let () = register "DIV32r" div32r

(* 0x6b,0xc0,0x10 *)
let imul32rri8 cpu ops =
  let _dst = unsigned cpu.reg ops.(0) in
  let _src = unsigned cpu.reg ops.(1) in
  let _imm = unsigned imm ops.(2) in
  RTL.[]

(* 0x8d,0x83,0x08,0xff,0xff,0xff *)
let lea32r cpu ops =
  let _src1 = unsigned cpu.reg ops.(0) in
  let _base  = unsigned cpu.reg_or_nil ops.(1) in
  let _index = unsigned imm ops.(2) in
  let _scale = unsigned cpu.reg_or_nil ops.(3) in
  let _disp = unsigned imm ops.(4) in
  RTL.[]

(* 0xc1,0xfe,0x02 *)
let sar32ri cpu ops =
  let _dst = unsigned cpu.reg ops.(0) in
  let _src = unsigned cpu.reg ops.(1) in
  let _imm = unsigned imm ops.(2) in
  RTL.[]

(* 0xc1,0xe0,0x02 *)
let shl32ri cpu ops =
  let _dst = unsigned cpu.reg ops.(0) in
  let _src = unsigned cpu.reg ops.(1) in
  let _imm = unsigned imm ops.(2) in
  RTL.[]

(* 0xc1,0xe8,0x02 *)
let shr32ri cpu ops =
  let _dst = unsigned cpu.reg ops.(0) in
  let _src = unsigned cpu.reg ops.(1) in
  let _imm = unsigned imm ops.(2) in
  RTL.[]

(* 0x83,0xec,0x1c *)
let sub32ri8 cpu ops =
  let _dst = unsigned cpu.reg ops.(0) in
  let _src = unsigned cpu.reg ops.(1) in
  let _imm = unsigned imm ops.(2) in
  RTL.[]

(* 0x29,0xc6 *)
let sub32rr cpu ops =
  let _dst = unsigned cpu.reg ops.(0) in
  let _src1 = unsigned cpu.reg ops.(1) in
  let _src2 = unsigned cpu.reg ops.(2) in
  RTL.[]


let () = register "IMUL32rri8" imul32rri8
let () = register "LEA32r" lea32r
let () = register "SAR32ri" sar32ri
let () = register "SHL32ri" shl32ri
let () = register "SHR32ri" shr32ri
let () = register "SUB32ri8" sub32ri8
let () = register "SUB32rr" sub32rr

(**
DIV32r divl %esi
IMUL32rri8 imull $0x10, %eax, %eax
LEA32r leal -0xf8(%ebx), %eax
SAR32ri sarl $0x2, %esi
SHL32ri shll $0x2, %eax
SHR32ri shrl $0x2, %eax
SUB32ri8 subl $0x1c, %esp
SUB32rr subl %eax, %esi
*)
