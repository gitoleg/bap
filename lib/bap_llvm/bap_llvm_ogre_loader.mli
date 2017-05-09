open Core_kernel.Std
open Bap.Std

module Elf_scheme : sig

  val off  : int64 Ogre.field
  val size : int64 Ogre.field
  val addr : int64 Ogre.field
  val name : string Ogre.field

  (** flags that describes an entry behavior *)
  val ld : bool Ogre.field (** loadable *)
  val r :  bool Ogre.field (** readable *)
  val w :  bool Ogre.field (** writable *)
  val x :  bool Ogre.field (** executable *)

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

module Loader : Image.Loader
