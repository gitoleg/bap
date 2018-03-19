open Core_kernel.Std
open Bap.Std

module Std = struct

  open Bap_rtl_kernel

  include Bap_rtl_helpers

  module RTL = struct
    include Bap_rtl_kernel
    include Exp
    include Infix
  end

  type rtl = RTL.rtl [@@deriving bin_io, compare, sexp]
  type exp = RTL.exp [@@deriving bin_io, compare, sexp]
  let bil_of_rtl = RTL.bil_of_t

  module Array = Op_array

  module Model = Bap_rtl_model

end
