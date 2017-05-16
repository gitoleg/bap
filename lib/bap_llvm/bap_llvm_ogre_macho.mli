
module Scheme : sig

  (** macho segment command *)
  val segment_cmd :
    ((string * int64 * int64), (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  val segment_cmd_flags :
    (string * (bool * bool * bool),
     (string -> bool -> bool -> bool -> 'a) -> 'a)
      Ogre.attribute

  val segment_cmd_mapping :
    ((string * int64 * int64), (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  (** macho section in file *)
  val macho_section :
    ((string * int64 * int64), (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  (** macho symbol  *)
  val macho_symbol :
    ((string * int64 * int64), (string -> int64 -> int64 -> 'a) -> 'a)
      Ogre.attribute

  (** macho symbol that is a function *)
  val function_ : (int64, (int64 -> 'a) -> 'a) Ogre.attribute
end

module Make(Fact : Ogre.S) : sig
  val image : unit Fact.t
end
