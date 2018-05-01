open Core_kernel.Std
open Bap.Std
open Bap_rtl.Std
open X86_env
open X86_cpu

module Reg_op = Reg

type cpu = {
  reg         : (op -> lhs exp) ec;
  reg_or_nil  : (op -> rhs exp) ec;
  whole       : (op -> lhs exp) ec;
  load        : 'a. 'a exp -> bitwidth -> rhs exp;
  store       : 'a 'b. 'a exp -> 'b exp -> bitwidth -> rtl;
  word_width  : bitwidth;
  sp_step     : rhs exp;
  sp          : lhs exp;
  rax         : lhs exp;
  rdx         : lhs exp;
  rbp         : lhs exp;
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
    let model = Array.fold ~init:model ~f:(add Cls.gpr) nums in
    Array.fold ~init:model ~f:(add Cls.vector) ymms

  let model = if Size.equal addr_size `r32 then model
    else
      let low x = RTL.low word (Exp.of_var x) in
      let model =
        List.fold ~init:model
          ~f:(fun m (n,r) -> Reg_model.Exp.add Cls.gpr (`Name n) (low r) m)
          [ "EBP", rbp; "ESP", rsp; "ESI", rsi; "EDI", rdi;
            "EIP", rip; "EAX", rax; "EBX", rbx;  "ECX", rcx;
            "EDX", rdx; ] in
      Array.fold nums ~init:model ~f:(fun m r ->
          let name = sprintf "%sD" (Var.name r) in
          Reg_model.Exp.add Cls.gpr (`Name name) (low r) m)

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

  let sp_step =
    let step = Size.in_bytes addr_size in
    let width = Size.in_bits addr_size in
    Exp.of_word Word.(of_int ~width step)

  let map = function
    | "EBP" -> "RBP"
    | "ESP" -> "RSP"
    | "ESI" -> "RSI"
    | "EDI" -> "RDI"
    | "EIP" -> "RIP"
    | "EAX" -> "RAX"
    | "EBX" -> "RBX"
    | "ECX" -> "RCX"
    | "EDX" -> "RDX"
    | "R8D"  -> "R8"
    | "R9D"  -> "R9"
    | "R10D" -> "R10"
    | "R11D" -> "R11"
    | "R12D" -> "R12"
    | "R13D" -> "R13"
    | "R14D" -> "R14"
    | "R15D" -> "R15"
    | s -> s

  let whole =
    if Size.equal addr_size `r32 then Reg_model.ec model
    else
      Ec.create (fun op ->
          let name = match op with
            | Reg r -> Reg.name r
            | _ -> failwith "register expected" in
          Reg_model.Exp.find_exn model (`Name (map name)))

  let make_cpu mem =
    let pc = Memory.min_addr mem in
    let word_width = if Size.equal addr_size `r32 then word
      else doubleword in
    let next = Addr.(pc + of_int ~width:addr_bits (Memory.length mem)) in {
      reg = Reg_model.ec model;
      reg_or_nil;
      whole;
      load;
      store;
      word_width;
      sp_step;
      sp  = Exp.of_var rsp;
      rax = Exp.of_var rax;
      rdx = Exp.of_var rdx;
      rbp = Exp.of_var rbp;
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

  let lift mem insn = lift (L.make_cpu mem) mem insn

  module CPU = IA32
end

module AMD64R = struct

  module R64 = struct
    include R64
    let addr_size = `r64
  end

  module L = Make(R64)

  let fails = String.Table.create ()

  let lift mem insn =
    let r = lift (L.make_cpu mem) mem insn in
    if Result.is_ok r then r
    else
      let insn = Insn.of_basic insn in
      Hashtbl.change fails (Insn.name insn)
        ~f:(function _ -> Some (Insn.asm insn));
      r

  let pr () =
    if Hashtbl.length fails <> 0 then
      printf "fails:\n";
    Hashtbl.iteri fails ~f:(fun ~key:name ~data:asm ->
        printf "%s %s\n" name asm)

  let () = at_exit pr

  module CPU = AMD64
end

open Ec
open Bitwidth
open Common

let push32r cpu ops =
  let src = unsigned cpu.reg ops.(0) in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := src;
    cpu.sp := cpu.sp - cpu.sp_step;
    cpu.store cpu.sp tmp word;
  ]

let push32i cpu ops =
  let src = unsigned imm ops.(0) in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := src;
    cpu.sp := cpu.sp - cpu.sp_step;
    cpu.store cpu.sp tmp word;
  ]

let push32rmm cpu ops =
  let base  = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp  = signed imm ops.(3) in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := cpu.load (base + scale * index + disp) word;
    cpu.sp := cpu.sp - cpu.sp_step;
    cpu.store cpu.sp tmp word;
  ]

(* 0x53 *)
let push64r cpu ops =
  let src = unsigned cpu.reg ops.(0) in
  let tmp = unsigned var doubleword in
  RTL.[
    tmp := src;
    cpu.sp := cpu.sp - cpu.sp_step;
    cpu.store cpu.sp tmp doubleword;
  ]

(* 6a 05 *)
let push64i32 cpu ops =
  let src = unsigned imm ops.(0) in
  let tmp = unsigned var doubleword in
  RTL.[
    tmp := src;
    cpu.sp := cpu.sp - cpu.sp_step;
    cpu.store cpu.sp tmp doubleword;
  ]

(* 0xff,0x35,0xa2,0x0a,0x20,0x00 *)
let push64rmm cpu ops =
  let base  = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp  = signed imm ops.(3) in
  let tmp = unsigned var doubleword in
  RTL.[
    tmp := cpu.load (base + scale * index + disp) doubleword;
    cpu.sp := cpu.sp - cpu.sp_step;
    cpu.store cpu.sp tmp doubleword;
  ]

let () = register "PUSH32r" push32r
let () = register "PUSHi32" push32i
let () = register "PUSH32rmm" push32rmm
let () = register "PUSH64i32" push64i32
let () = register "PUSH64r" push64r
let () = register "PUSH64rmm" push64rmm

(** TODO: redo!
    this a bad approach, but it works for 0x41,0x89,0xff amd x86-64 *)
let mov_rr cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned cpu.reg ops.(1) in
  let dst' = unsigned cpu.whole ops.(0) in
  RTL.[
    dst' := zero;
    dst := src ]

let mov_rm cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let base = unsigned cpu.reg_or_nil ops.(1) in
  let index = unsigned imm ops.(2) in
  let scale = unsigned cpu.reg_or_nil ops.(3) in
  let disp  = signed imm ops.(4) in
  RTL.[
    dst := cpu.load (base + scale * index + disp) cpu.word_width;
  ]

let mov_ri cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned imm ops.(1) in
  RTL.[ dst := src ]

let mov_mi cpu ops =
  let base = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp  = signed imm ops.(3) in
  let x = unsigned imm ops.(5) in
  RTL.[
    cpu.store (base + scale * index + disp) x cpu.word_width;
  ]

let mov_mr cpu ops =
  let base = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp  = signed imm ops.(3) in
  let x = unsigned cpu.reg ops.(5) in
  RTL.[
    cpu.store (base + scale * index + disp) x word;
  ]

let mov8_mi cpu ops =
  let base = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp  = signed imm ops.(3) in
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
  let disp  = signed imm ops.(4) in
  RTL.[
    dst := cpu.load (base + scale * index + disp) byte;
  ]

(** todo movsx*8 .. is it a byte that we need to move? *)
let movsx32rr8 cpu ops =
  let dst = signed cpu.reg ops.(0) in
  let src = signed cpu.reg ops.(1) in
  RTL.[
    dst := src;
  ]

let movsx64rr8 cpu ops =
  let dst = signed cpu.reg ops.(0) in
  let src = signed cpu.reg ops.(1) in
  RTL.[
    dst := low byte src;
  ]


let movsx64rr32 cpu ops =
  let dst = signed cpu.reg ops.(0) in
  let src = signed cpu.reg ops.(1) in
  RTL.[
    dst := low word src;
  ]

let () = register "MOV32rr" mov_rr
let () = register "MOV32rm" mov_rm
let () = register "MOV32ri" mov_ri
let () = register "MOV32mi" mov_mi
let () = register "MOV32mr" mov_mr
let () = register "MOV8mi" mov8_mi
let () = register "MOV32ao32" mov32_ao_32
let () = register "MOVZX32rm8" movzx32rm8
let () = register "MOVSX32rr8" movsx32rr8

let () = register "MOV64mi32" mov_mi
let () = register "MOV64mr" mov_mr
let () = register "MOV64ri32" mov_ri
let () = register "MOV64rm" mov_rm
let () = register "MOV64rr" mov_rr
let () = register "MOVSX64rr8" movsx64rr8
let () = register "MOVSX64rr32" movsx64rr32

let test_rr cpu ops =
  let op1 = unsigned cpu.reg ops.(0) in
  let op2 = unsigned cpu.reg ops.(1) in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := op1 land op2;
    oF := zero;
    cf := zero;
    sf := msb tmp;
    zf := tmp = zero;
  ]

let test8_rr cpu ops =
  let op1 = unsigned cpu.reg ops.(0) in
  let op2 = unsigned cpu.reg ops.(1) in
  let tmp = unsigned var byte in
  RTL.[
    tmp := op1 land op2;
    oF := zero;
    cf := zero;
    sf := msb tmp;
    zf := tmp = zero;
  ]

let () = register "TEST32rr" test_rr
let () = register "TEST64rr" test_rr
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
  let disp = signed imm ops.(3) in
  let _xz = unsigned cpu.reg_or_nil ops.(4) in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := cpu.load (base + scale * index + disp) cpu.word_width;
    cpu.sp := cpu.sp - cpu.sp_step;
    cpu.store cpu.sp cpu.next cpu.word_width;
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

(* 0x7c, 0x0a *)
let jl cpu ops =
  let imm = signed imm ops.(0) in
  RTL.[
    when_ (sf lxor oF) [
      jmp (cpu.next + imm);
    ]
  ]

(* eb,0a  *)
let jmp cpu ops =
  let disp = signed imm ops.(0) in
  RTL.[jmp (cpu.next + disp)]

(* 0xff,0xe0  *)
let jmp_r cpu ops =
  let reg = unsigned cpu.reg ops.(0) in
  RTL.[jmp reg]

(* 0xff,0x25,0x28,0xa0,0x04,0x08 *)
let jmp_m cpu ops =
  let base  = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp = signed imm ops.(3) in
  RTL.[
    jmp (cpu.load (base + scale * index + disp) cpu.word_width)
  ]

(* 77, 0a *)
let ja cpu ops =
  let imm = signed imm ops.(0) in
  RTL.[
    when_ (lnot (cf lor zf)) [
      jmp (cpu.next + imm);
    ]
  ]

(* e8 0a 0a 0a 0a *)
let call_pcrel32 cpu ops =
  let disp = signed imm ops.(0) in
  RTL.[
    cpu.sp := cpu.sp - cpu.sp_step;
    cpu.store cpu.sp cpu.next cpu.word_width;
    jmp (cpu.next + disp)
  ]

(* c9  *)
let leave cpu _ops =
  RTL.[
    cpu.sp := cpu.rbp;
    cpu.rbp := cpu.load cpu.sp cpu.word_width;
    cpu.sp := cpu.sp + cpu.sp_step;
  ]

(* c3 *)
let ret cpu _ops =
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := cpu.load cpu.sp cpu.word_width;
    cpu.sp := cpu.sp + cpu.sp_step;
    jmp tmp;
  ]

(* 0xff,0xd2 *)
let call32r cpu ops =
  let src = unsigned cpu.reg ops.(0) in
  let tmp = unsigned var word in
  RTL.[
    tmp := src;
    cpu.sp := cpu.sp - cpu.sp_step;
    cpu.store cpu.sp cpu.next word;
    jmp tmp;
  ]

(* 0x41,0xff,0x14,0xdc *)
let call64m cpu ops =
  let base  = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp = signed imm ops.(3) in
  let _xz = unsigned cpu.reg_or_nil ops.(4) in
  let tmp = unsigned var doubleword in
  RTL.[
    tmp := cpu.load (base + scale * index + disp) doubleword;
    cpu.sp := cpu.sp - cpu.sp_step;
    cpu.store cpu.sp cpu.next doubleword;
    jmp tmp;
  ]

(* 0xe8,0x0a,0x0a,0x0a,0x0a *)
let call64pcrel32 cpu ops =
  let disp = signed imm ops.(0) in
  RTL.[
    cpu.sp := cpu.sp - cpu.sp_step;
    cpu.store cpu.sp cpu.next doubleword;
    jmp (cpu.next + disp)
  ]


(* 0xff,0xd0 *)
let call64r cpu ops =
  let src = unsigned cpu.reg ops.(0) in
  let tmp = unsigned var doubleword in
  RTL.[
    tmp := src;
    cpu.sp := cpu.sp - cpu.sp_step;
    cpu.store cpu.sp cpu.next doubleword;
    jmp tmp;
  ]


let () = register "CALLpcrel32" call_pcrel32
let () = register "JA_1" ja
let () = register "JNE_1" jne
let () = register "JL_1" jl
let () = register "JL_4" jl
let () = register "JMP_1" jmp
let () = register "JMP_4" jmp
let () = register "JMP64m" jmp_m
let () = register "JMP64r" jmp_r
let () = register "JE_1" je
let () = register "JMP32m" jmp_m
let () = register "CALL32m" call32m
let () = register "CALL32r" call32r
let () = register "LEAVE" leave
let () = register "LEAVE64" leave
let () = register "RETL" ret
let () = register "RETQ" ret
let () = register "CALL64m" call64m
let () = register "CALL64pcrel32" call64pcrel32
let () = register "CALL64r" call64r

(* 0x83,0x45,0xc0,0x01 *)
let add32mi8 cpu ops =
  let base = unsigned cpu.reg ops.(0) in
  let scale = unsigned imm ops.(1) in
  let index = unsigned cpu.reg_or_nil ops.(2) in
  let disp = signed imm ops.(3) in
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

(* 0x48,0x83,0x45,0xf8,0x08  *)
let add64mi8 cpu ops =
  let base = unsigned cpu.reg ops.(0) in
  let scale = unsigned imm ops.(1) in
  let index = unsigned cpu.reg_or_nil ops.(2) in
  let disp = signed imm ops.(3) in
  let _xz = unsigned cpu.reg_or_nil ops.(4) in
  let imm = unsigned fixed_imm byte ops.(5) in
  let addr = unsigned var doubleword in
  let tmp1 = unsigned var doubleword in
  let tmp2 = unsigned var doubleword in
  let resl = unsigned var doubleword in
  RTL.[
    addr := base + scale * index + disp;
    tmp1 := cpu.load addr doubleword;
    tmp2 := imm;
    resl := tmp1 + tmp2;
    cpu.store addr resl doubleword;
    zf := resl = zero;
    sf := msb resl;
    cf := resl < tmp1;
    oF := (msb tmp1 = msb tmp2) land (msb tmp1 lxor msb resl);
  ]

(* 0x48,0x83,0xc3,0x01 *)
let add64ri8 cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned cpu.reg ops.(1) in
  let imm = unsigned fixed_imm byte ops.(2) in
  let tmp1 = unsigned var doubleword in
  let tmp2 = unsigned var doubleword in
  RTL.[
    tmp1 := src;
    tmp2 := imm;
    dst := src + imm;
    zf := dst = zero;
    sf := msb dst;
    cf := dst < tmp1;
    oF := (msb tmp1 = msb tmp2) land (msb tmp1 lxor msb dst);
  ]

(* 0x48,0x01,0xd0 *)
let add64rr cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src1 = unsigned cpu.reg ops.(1) in
  let src2 = unsigned cpu.reg ops.(2) in
  let tmp1 = unsigned var doubleword in
  let tmp2 = unsigned var doubleword in
  RTL.[
    tmp1 := src1;
    tmp2 := src2;
    dst := src1 + src2;
    zf := dst = zero;
    sf := msb dst;
    cf := dst < tmp1;
    oF := (msb tmp1 = msb tmp2) land (msb tmp1 lxor msb dst);
  ]

let () = register "ADD32mi8" add32mi8
let () = register "ADD32ri"  add32ri
let () = register "ADD32ri8" add32ri8
let () = register "ADD32rr"  add32rr
let () = register "ADD64mi8" add64mi8
let () = register "ADD64ri8" add64ri8
let () = register "ADD64rr" add64rr


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

(* 0x48,0x83,0xe4,0xf0 *)
let and64ri8 cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned cpu.reg ops.(1) in
  let imm = unsigned fixed_imm byte ops.(2) in
  let tmp = signed var doubleword in
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
let () = register "AND64ri8" and64ri8
let () = register "XOR32rr" xor32rr

(* 0x3b,0x45,0xcc  *)
(* todo: check of flag *)
let cmp32rm cpu ops =
  let reg = unsigned cpu.reg ops.(0) in
  let base  = unsigned cpu.reg_or_nil ops.(1) in
  let index = unsigned imm ops.(2) in
  let scale = unsigned cpu.reg_or_nil ops.(3) in
  let disp = signed imm ops.(4) in
  let src1 = unsigned var word in
  let src2 = unsigned var word in
  let tmp = unsigned var word in
  RTL.[
    src1 := low word reg;
    src2 := cpu.load (base + scale * index + disp) word;
    tmp := src1 - src2;
    cf := src1 < src2;
    sf := msb tmp;
    zf := tmp = zero;
    oF := msb ((src1 lxor src2) land (src1 lxor tmp));
  ]

(* 0x39,0xf7 *)
let cmp_rr cpu ops =
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

(* 0x80,0x3d,0x80,0xa0,0x04,0x08,0x00 *)
let cmp8mi cpu ops =
  let base  = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp = signed imm ops.(3) in
  let imm = unsigned fixed_imm byte ops.(5) in
  let src = unsigned var byte in
  let tmp = unsigned var byte in
  RTL.[
    src := cpu.load (base + scale * index + disp) byte;
    tmp := src - imm;
    cf := src < imm;
    oF := msb (src lxor imm) land (src lxor tmp);
    sf := msb tmp;
    zf := tmp = zero;
  ]

(* 0x83,0xf8,0x06 *)
let cmp_ri8 cpu ops =
  let src1 = unsigned cpu.reg ops.(0) in
  let src2 = unsigned fixed_imm byte ops.(1) in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := src1 - src2;
    cf := src1 < src2;
    sf := msb tmp;
    zf := tmp = zero;
    oF := (src1 lxor src2) land (src1 lxor tmp);
  ]

(* 0x48,0x83,0x3d,0x58,0x07,0x20,0x00,0x00 *)
let cmp_mi8 cpu ops =
  let base  = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp = signed imm ops.(3) in
  let imm = unsigned fixed_imm byte ops.(5) in
  let src = unsigned var cpu.word_width in
  let tmp = unsigned var byte in
  RTL.[
    src := cpu.load (base + scale * index + disp) cpu.word_width;
    tmp := src - imm;
    cf := src < imm;
    oF := msb (src lxor imm) land (src lxor tmp);
    sf := msb tmp;
    zf := tmp = zero;
  ]

let () = register "CMP32rm" cmp32rm
let () = register "CMP32rr" cmp_rr
let () = register "CMP8mi" cmp8mi
let () = register "CMP32ri8" cmp_ri8
let () = register "CMP64rr" cmp_rr
let () = register "CMP64ri8" cmp_ri8
let () = register "CMP64mi8" cmp_mi8

(* f4 *)
let hlt _cpu _ops = []

(* 90 *)
let noop _cpu _ops = []

let () = register "HLT" hlt
let () = register "NOOP" noop
let () = register "NOOPL" noop
let () = register "NOOPW" noop

(* 5d *)
let pop32r cpu ops =
  let reg = unsigned cpu.reg ops.(0) in
  RTL.[
    reg := cpu.load cpu.sp cpu.word_width;
    cpu.sp := cpu.sp + cpu.sp_step;
  ]

(* 5d *)
let pop64r cpu ops =
  let reg = unsigned cpu.reg ops.(0) in
  RTL.[
    reg := cpu.load cpu.sp cpu.word_width;
    cpu.sp := cpu.sp + cpu.sp_step;
  ]

let () = register "POP32r" pop32r
let () = register "POP64r" pop64r

(* f7 f6 OR  0x48,0xf7,0xf3 *)
(* todo: add some cpu exn here  *)
let div32_r cpu ops =
  let reg = unsigned cpu.reg ops.(0) in
  let div = unsigned var doubleword in
  let rem = unsigned var doubleword in
  RTL.[
    div := (cpu.rdx ^ cpu.rax) / reg;
    rem := (cpu.rdx ^ cpu.rax) % reg;
    cpu.rax := div;
    cpu.rdx := rem;
  ]

let div64_r cpu ops =
  let reg = unsigned cpu.reg ops.(0) in
  let div = unsigned var quadword in
  let rem = unsigned var quadword in
  RTL.[
    div := (cpu.rdx ^ cpu.rax) / reg;
    rem := (cpu.rdx ^ cpu.rax) % reg;
    cpu.rax := div;
    cpu.rdx := rem;
  ]

let () = register "DIV32r" div32_r
let () = register "DIV64r" div64_r

(* 0x6b,0xc0,0x10 *)
let imul32rri8 cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned cpu.reg ops.(1) in
  let imm = unsigned imm ops.(2) in
  let tmp1 = unsigned var doubleword in
  let tmp2 = unsigned var doubleword in
  RTL.[
    tmp1 := src;
    tmp2 := tmp1 * imm;
    dst := low word tmp2 ;
    oF := tmp2 <> dst;
    cf := oF;
  ]

(* 0x48,0x6b,0xc0,0x10 *)
let imul64rri8 cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned cpu.reg ops.(1) in
  let imm = unsigned imm ops.(2) in
  let tmp1 = unsigned var quadword in
  let tmp2 = unsigned var quadword in
  RTL.[
    tmp1 := src;
    tmp2 := tmp1 * imm;
    dst := low doubleword tmp2;
    oF := tmp2 <> dst;
    cf := oF;
  ]

(* 0x8d,0x83,0x08,0xff,0xff,0xff *)
let lea_r cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let base  = unsigned cpu.reg_or_nil ops.(1) in
  let index = unsigned imm ops.(2) in
  let scale = unsigned cpu.reg_or_nil ops.(3) in
  let disp = signed imm ops.(4) in
  RTL.[
    dst := base + scale * index + disp;
  ]

(* 0x8d,0x50,0xff *)
let lea6432r cpu ops =
  let dst = unsigned cpu.whole ops.(0) in
  let base  = unsigned cpu.reg_or_nil ops.(1) in
  let index = unsigned imm ops.(2) in
  let scale = unsigned cpu.reg_or_nil ops.(3) in
  let disp = signed imm ops.(4) in
  let tmp = unsigned const word 0 in
  RTL.[
    dst := tmp ^ low word (base + scale * index + disp);
  ]

(* 0xc1,0xfe,0x02 *)
(* todo: check cf here *)
let sar_ri cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = signed cpu.reg ops.(1) in
  let imm = unsigned imm ops.(2) in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := dst;
    dst := src >> imm;
    cf := lsb (tmp >> (imm - one));
    zf := dst = zero;
    sf := msb dst;
  ]

(* d1,f8 OR 0x48,0xd1,0xf8  *)
let sar_r1 cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = signed cpu.reg ops.(1) in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := src;
    dst := src >> one;
    cf := lsb tmp ;
    zf := dst = zero;
    sf := msb dst;
  ]

(* 0xc1,0xe0,0x02 *)
(* todo: check cf here *)
let shl_ri cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned cpu.reg ops.(1) in
  let imm = unsigned imm ops.(2) in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := dst;
    dst := src << imm;
    cf := msb (tmp << (imm - one)) ;
    zf := dst = zero;
    sf := msb dst;
  ]

(* 0xc1,0xe8,0x02 *)
(* todo: check cf here *)
let shr_ri cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned cpu.reg ops.(1) in
  let imm = unsigned imm ops.(2) in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := dst;
    dst := src >> imm;
    cf := lsb (tmp >> (imm - one)) ;
    zf := dst = zero;
    sf := msb dst;
  ]

(* 0x83,0xec,0x1c *)
let sub_ri8 cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned cpu.reg ops.(1) in
  let imm = unsigned imm ops.(2) in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := src;
    dst := src - imm;
    cf := tmp < imm;
    zf := dst = zero;
    sf := msb dst;
    oF := (tmp lxor imm) land (tmp lxor dst);
  ]

(* 0x29,0xc6 *)
let sub_rr cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src1 = unsigned cpu.reg ops.(1) in
  let src2 = unsigned cpu.reg ops.(2) in
  let tmp1 = unsigned var cpu.word_width in
  let tmp2 = unsigned var cpu.word_width in
  RTL.[
    tmp1 := src1;
    tmp2 := src2;
    dst := src1 - src2;
    cf := tmp1 < tmp2;
    zf := dst = zero;
    sf := msb dst;
    oF := (tmp1 lxor tmp2) land (tmp1 lxor dst);
  ]

(* 0x2b,0x05,0A,0A,0A,0A *)
let sub32rm cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src1 = unsigned cpu.reg ops.(1) in
  let base  = unsigned cpu.reg_or_nil ops.(2) in
  let index = unsigned imm ops.(3) in
  let scale = unsigned cpu.reg_or_nil ops.(4) in
  let disp = signed imm ops.(5) in
  let tmp1 = unsigned var cpu.word_width in
  let tmp2 = unsigned var cpu.word_width in
  RTL.[
    tmp1 := src1;
    tmp2 := cpu.load (base + scale * index + disp) word;
    dst := tmp1 - tmp2;
    cf := tmp1 < tmp2;
    zf := dst = zero;
    sf := msb dst;
    oF := (tmp1 lxor tmp2) land (tmp1 lxor dst);
  ]

(* 0x2d,0x80,0xa0,0x04,0x08 *)
let sub_i32 cpu ops =
  let imm = unsigned imm ops.(0) in
  let tmp = unsigned var cpu.word_width in
  RTL.[
    tmp := cpu.rax;
    cpu.rax := cpu.rax - imm;
    cf := tmp < imm;
    zf := cpu.rax = zero;
    sf := msb cpu.rax;
    oF := (tmp lxor imm) land (tmp lxor cpu.rax);
  ]

let () = register "LEA32r" lea_r
let () = register "LEA64r" lea_r
let () = register "LEA64_32r" lea6432r
let () = register "IMUL32rri8" imul32rri8
let () = register "IMUL64rri8" imul64rri8
let () = register "SAR32ri" sar_ri
let () = register "SAR32r1" sar_r1
let () = register "SHL32ri" shl_ri
let () = register "SHR32ri" shr_ri
let () = register "SUB32ri8" sub_ri8
let () = register "SUB32rr" sub_rr
let () = register "SUB32rm" sub32rm
let () = register "SUB32i32" sub_i32
let () = register "SAR64r1" sar_r1
let () = register "SAR64ri" sar_ri
let () = register "SHL64ri" shl_ri
let () = register "SHR64ri" shr_ri
let () = register "SUB64i32" sub_i32
let () = register "SUB64ri8" sub_ri8
let () = register "SUB64rr" sub_rr

(* 0x48,0x98 *)
let cdqe cpu _ops =
  let tmp = signed var cpu.word_width in
  RTL.[
    tmp := low word cpu.rax;
    cpu.rax := tmp;
  ]

let () = register "CDQE" cdqe
