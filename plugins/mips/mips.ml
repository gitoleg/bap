open Core_kernel.Std
open Bap.Std
open Bap_rtl.Std

(* This CPU model and instruction set is based on the
 * "MIPS Architecture For Programmers
 * Volume II-A: The MIPS64 Instruction Set Reference Manual"
 * Document Number: MD00087  Revision 6.04
 * November 13, 2015 *)

module Model = Mips_model

module Std = struct
  include Mips_utils
  include Mips_types
  include Mips_cpu

  include Ec
  include Bitwidth

  module RTL = RTL

  type lift = cpu -> op array -> rtl list

  module Cpu = struct
    type t = cpu

    let update c addr =
      let cia = Exp.(signed (of_word addr)) in
      {c with cia}
  end

  include Lifter_model.Make(Cpu)

  let (>>) = register

  let lift addr_size endian mem insn =
    init (make_cpu addr_size endian mem);
    lifter mem insn

  module M32BE = struct
    module CPU = Model.MIPS_32_cpu
    let lift = lift `r32 BigEndian
  end

  module M32LE = struct
    module CPU = Model.MIPS_32_cpu
    let lift = lift `r32 LittleEndian
  end

  module M64BE = struct
    module CPU = Model.MIPS_64_cpu
    let lift = lift `r64 BigEndian
  end

  module M64LE = struct
    module CPU = Model.MIPS_64_cpu
    let lift = lift `r64 LittleEndian
  end

  include Model

end
