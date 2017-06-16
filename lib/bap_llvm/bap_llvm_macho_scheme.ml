open Core_kernel.Std
open Ogre.Type
open Bap_llvm_ogre_types.Scheme

(** macho segment command *)
let segment_cmd () =
  Ogre.declare ~name:"segment-command" (scheme name $ off $ size)
    Tuple.T3.create

let segment_cmd_flags () =
  Ogre.declare ~name:"segment-command-flags"
    (scheme name $ r $ w $ x)
    (fun name r w x -> name, (r,w,x))

let virtual_segment_cmd () =
  Ogre.declare ~name:"virtual-segment-command"
    (scheme name $ addr $ size) Tuple.T3.create

(** macho section *)
let macho_section () =
  Ogre.declare ~name:"macho-section" (scheme name $ addr $ size)
    Tuple.T3.create

(** macho symbol that doesn't belong to any section *)
let macho_symbol () =
  Ogre.declare ~name:"macho-symbol" (scheme name $ value)
    Tuple.T2.create

(** macho symbol defined in some section *)
let macho_section_symbol () =
  Ogre.declare ~name:"macho-section-symbol" (scheme name $ addr $ size)
    Tuple.T3.create

(** macho symbol that is a function *)
let function_ () = Ogre.declare ~name:"function" (scheme addr $ name) Tuple.T2.create
