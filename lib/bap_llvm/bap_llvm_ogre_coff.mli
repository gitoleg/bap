
module Scheme : sig

  val section_header :
    ((string * int64 * int64), (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  val virtual_section_header :
    ((string * int64 * int64), (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  val section_flags :
    (string * (bool * bool * bool),
     (string -> bool -> bool -> bool -> 'a) -> 'a)
      Ogre.attribute

  val code_content : (string, (string -> 'a) -> 'a) Ogre.attribute

  val symbol :
    ((string * int64 * int64), (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  val function_ : (int64, (int64 -> 'a) -> 'a) Ogre.attribute
end

module Make(Fact : Ogre.S) : sig
  val image : unit Fact.t
  val probe : bool Fact.t
end
