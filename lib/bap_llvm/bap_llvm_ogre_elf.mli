open Monads.Std

module Scheme : sig

  val ld : bool Ogre.field (** loadable *)

  (** elf program header as it is in file *)
  val program_header :
    ((string * int64 * int64), (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  (** elf program header as it is in memory *)
  val virtual_program_header :
    ((string * int64 * int64), (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  (** elf program header flags *)
  val program_header_flags :
    (string * bool * bool * bool * bool,
     (string -> bool -> bool -> bool -> bool -> 'a) -> 'a)
      Ogre.attribute

  (** elf section header *)
  val section_header :
    ((string * int64 * int64), (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  (** elf symbol entry *)
  val symbol_entry :
    (string * int64 * int64, (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  (** elf symbols that are functions *)
  val code_entry : (int64, (int64 -> 'a) -> 'a) Ogre.attribute

end

module Make(Fact : Ogre.S) : sig
  val image : unit Fact.t
end
