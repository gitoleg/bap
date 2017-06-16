open Core_kernel.Std
open Ogre.Type
open Bap_llvm_ogre_types.Scheme

(** coff section in file *)
let section_header () =
  Ogre.declare ~name:"section-header" (scheme name $ off $ size)
    Tuple.T3.create

(** coff section in memory *)
let virtual_section_header () =
  Ogre.declare ~name:"virtual-section-header"
    (scheme name $ addr $ size) Tuple.T3.create

(** coff section access flags *)
let section_flags () =
  Ogre.declare ~name:"section-flags" (scheme name $ r $ w $ x)
    (fun name r w x -> name, (r,w,x))

(** coff section that contains code *)
let code_content () = Ogre.declare ~name:"code-content" (scheme name) ident

(** coff symbol  *)
let symbol () =
  Ogre.declare ~name:"symbol" (scheme name $ addr $ size) Tuple.T3.create

(** coff symbol that is a function *)
let function_ () =
  Ogre.declare ~name:"function" (scheme addr $ name) Tuple.T2.create
