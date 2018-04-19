open Core_kernel.Std
open Bap.Std
open Bap_rtl.Std

open Powerpc_types

module Shadow_rtl = struct
  open Bitwidth
  let foreach = RTL.foreach'

  let first e bits =
    let w = Exp.width e in
    RTL.extract (w - 1) (w - bits) e

  let last e bits = RTL.extract (bits - 1) 0 e
  let high w e = first e (int_of_bitwidth w)
  let low w e = last e (int_of_bitwidth w)

  let msb e =
    let h = Exp.width e - 1 in
    RTL.extract h h e

  let lsb e = RTL.extract 0 0 e

  let nth w e index =
    let width = Exp.width e in
    let step = int_of_bitwidth w in
    let x = width / step - index - 1 in
    let hi = (x + 1) * step - 1 in
    let lo = x * step in
    let n = width / step in
    let hi, lo =
      if n * step < width then
        let sh = width - n * step in
        hi + sh, lo + sh
      else hi, lo in
    RTL.extract hi lo e

  let extract left right e =
    let width = Exp.width e in
    let target_width = right - left + 1 in
    if width >= target_width then
      let hi = width - left - 1 in
      let lo = width - right - 1 in
      RTL.extract hi lo e
    else
      RTL.extract (target_width - 1) 0 e
end

module Std = struct
  include Ec
  include Bitwidth
  include Powerpc_utils
  include Powerpc_cpu
  include Powerpc_types

  module Array = Array

  module RTL = struct
    include RTL
    include Shadow_rtl
  end

  type lift = cpu -> op array -> rtl list

  let concat f g = fun cpu ops -> f cpu ops @ g cpu ops

  let (^) = concat

  let width e =
    let w = Exp.width e in
    Exp.of_word (Word.of_int ~width:w w)

  let dot fc cpu ops =
    let res = signed cpu.reg ops.(0) in
    let x = signed var cpu.word_width in
    fc cpu ops @
    RTL.[
      x := low cpu.word_width res;
      nth bit cpu.cr 0 := x < zero;
      nth bit cpu.cr 1 := x > zero;
      nth bit cpu.cr 2 := x = zero;
    ]

  module Cpu = struct
    type t = cpu

    let update c addr =
      let pc = Exp.(signed (of_word addr)) in
      {c with pc}
  end

  include Lifter_model.Make(Cpu)

  let register_dot name lifter = register name (dot lifter)
  let (>|) = register
  let (>.) = register_dot

  let lift addr_size endian mem insn =
    init (make_cpu addr_size endian mem);
    lifter mem insn

  include Powerpc_model

  module T32 = struct
    module CPU = PowerPC_32_cpu
    let lift = lift `r32 BigEndian
  end

  module T64 = struct
    module CPU = PowerPC_64_cpu
    let lift = lift `r64 BigEndian
  end

  module T64_le = struct
    module CPU = PowerPC_64_cpu
    let lift = lift `r64 LittleEndian
  end

end
