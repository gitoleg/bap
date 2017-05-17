
module Scheme : sig
  (** coff section in file *)
  val section_header :
    (string * int64 * int64, (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  (** coff section in memory *)
  val virtual_section_header :
    (string * int64 * int64, (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  (** coff section access flags *)
  val section_flags :
    (string * (bool * bool * bool),
     (string -> bool -> bool -> bool -> 'a) -> 'a)
      Ogre.attribute

  (** coff section that contains code *)
  val code_content : (string, (string -> 'a) -> 'a) Ogre.attribute

  (** coff symbol *)
  val symbol :
    (string * int64 * int64, (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  (** coff symbol is function *)
  val function_ : (string * int64, (string -> int64 -> 'a) -> 'a) Ogre.attribute
end

module Make(Fact : Ogre.S) : sig
  val image : unit Fact.t
end
