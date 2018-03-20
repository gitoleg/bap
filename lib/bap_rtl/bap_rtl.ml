open Core_kernel.Std
open Bap.Std

module Translate = Bap_rtl_translate
module Helpers = Bap_rtl_helpers
module Exp_constructor = Bap_rtl_exp.Constructor
module Bitwidth = Bap_rtl_bitwidth

module Infix = struct
  include Bap_rtl_exp.Infix
  include Bap_rtl_core.Infix
end

module Std = struct

  include Bap_rtl_types

  module Exp = Bap_rtl_exp.Exp

  module RTL = struct
    let load = Exp.load
    let extract = Exp.extract
    include Bap_rtl_core.Rtl
    include Infix
  end

  type 'a ec = 'a Bap_rtl_exp.ec

  include Exp_constructor
  include Helpers
  include Bitwidth

  let bil_of_rtl rtl =
    Helpers.norm_jumps @@
    Translate.run rtl

  module Mem_model = Bap_rtl_model

end
