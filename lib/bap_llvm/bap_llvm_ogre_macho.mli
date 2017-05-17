
module Scheme : sig

  val value : int64 Ogre.field

  (** macho segment command *)
  val segment_cmd :
    (string * int64 * int64, (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  val segment_cmd_flags :
    (string * (bool * bool * bool),
     (string -> bool -> bool -> bool -> 'a) -> 'a)
      Ogre.attribute

  val virtual_segment_cmd :
    (string * int64 * int64, (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  (** macho section *)
  val macho_section :
    (string * int64 * int64 * int64, (string -> int64 -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  (** macho symbol, that doesn't belong to any section *)
  val macho_symbol :
    (string * int64, (string -> int64 -> 'a) -> 'a) Ogre.attribute

  (** macho symbol that is defined in some section *)
  val macho_section_symbol :
    (string * int64 * int64, (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  (** macho symbol that is a function *)
  val function_ : (string * int64, (string -> int64 -> 'a) -> 'a) Ogre.attribute
end

module Make(Fact : Ogre.S) : sig
  val image : unit Fact.t
end
