open Core_kernel.Std
open Ogre.Type
open Bap_llvm_ogre_types.Scheme


(** flags that describes an entry behavior *)
let ld = "load" %: bool

let declare name scheme f = Ogre.declare ~name scheme f

(** elf program header as it is in file *)
let program_header () = declare "program-header"
    (scheme name $ off $ size) Tuple.T3.create

(** elf program header as it is in memory *)
let virtual_program_header () = declare "virtual-program-header"
    (scheme name $ addr $ size) Tuple.T3.create

(** elf program header flags *)
let program_header_flags () = declare "program-header-flags"
    (scheme name $ ld $ r $ w $ x) (fun name ld r w x -> name,ld,r,w,x)

(** elf section header *)
let section_header () = declare "section-header"
    (scheme name $ addr $ size $ off)
    (fun name addr size off -> name,addr,size,off)

let section_flags () = declare "section-flags"
    (scheme name $ w $ x) Tuple.T3.create

(** elf symbol entry *)
let symbol_entry () =
  declare "symbol-entry" (scheme name $ addr $ size) Tuple.T3.create

(** elf symbols that are functions *)
let code_entry () = declare "code-entry" (scheme addr) ident

let is_relocatable () = declare "relocatable" (scheme flag) ident
