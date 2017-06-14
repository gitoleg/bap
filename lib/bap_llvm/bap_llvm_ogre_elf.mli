
include Bap_llvm_ogre_types.Rules

module type E = sig
  val entry : int64
end

module Relocatable(E : E) : Bap_llvm_ogre_types.Rules
