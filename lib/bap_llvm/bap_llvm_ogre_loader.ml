open Core_kernel.Std
open Monads.Std

module Fact = Ogre.Make(Monad.Ident)
module Result = Monad.Result.Error
open Result.Syntax

module Elf_scheme = struct
  open Ogre.Type

  let declare name scheme f = Ogre.declare ~name scheme f

  (** p stands for physical, v - for virtual  *)
  let p_off = "p-offset"  %: int
  let p_size = "p-size"  %: int
  let p_addr = "p-addr"  %: int
  let v_size = "v-size"  %: int
  let v_addr = "v-addr"  %: int
  let e_type = "entry-type" %: int
  let index = "index" %: int
  let name  = "name"   %: str
  let flags = "flags"  %: int
  let align = "align"  %: int

  module Phdr = struct
    type t = {
      p_type   : int64;
      p_offset : int64;
      p_vaddr  : int64;
      p_paddr  : int64;
      p_filesz : int64;
      p_memsz  : int64;
      p_flags  : int64;
      p_align  : int64;
    } [@@deriving fields]

    let t () =
      declare "program-header"
        (scheme
           e_type $ p_off $ v_addr $ p_addr $ p_size $ v_size
         $ flags $ align)
        (fun p_type p_offset p_vaddr p_paddr p_filesz p_memsz p_flags p_align ->
           Fields.create ~p_type ~p_offset ~p_vaddr ~p_paddr ~p_filesz ~p_memsz ~p_flags ~p_align)
  end

  module Shdr = struct
    type t = {
      sh_name : int64;
      sh_type : int64;
      sh_flags : int64;
      sh_addr : int64;
      sh_offset : int64;
      sh_size : int64;
      sh_link : int64;
      sh_info : int64;
      sh_addralign : int64;
      sh_entsize : int64;
    } [@@deriving fields]

    let sec_name = "sec-name" %: str
    let sh_link = "sh-link" %: int
    let sh_info = "sh-info" %: int
    let sh_entsize = "sh-entsize" %: int

    let t () =
      declare "section-header"
        (scheme index $ e_type $ flags $ v_addr $ p_off
         $ p_size $ sh_link $ sh_info $ align $ sh_entsize)
        (fun sh_name sh_type sh_flags sh_addr sh_offset sh_size sh_link
          sh_info sh_addralign sh_entsize ->
          Fields.create
            ~sh_name ~sh_type ~sh_flags ~sh_addr ~sh_offset ~sh_size ~sh_link
            ~sh_info ~sh_addralign ~sh_entsize)

    let string_table () =
      declare "string-table"
        (scheme sec_name $ index $ name)
        (fun sec_name index name -> sec_name, index, name)
  end

  module Sym = struct
    type t = {
      st_name  : int64;
      st_value : int64;
      st_size  : int64;
      st_info  : int64;
      st_other : int64;
      st_shndx : int64;
    } [@@deriving fields]


    let value  = "value"   %: int
    let st_info = "st-info" %: int
    let st_other = "st-other" %: int
    let st_shndx = "st-shndx" %: int

    let t () =
      declare "symbol-entry"
        (scheme index $ value $ p_size $ st_info $ st_other $ st_shndx)
        (fun st_name st_value st_size st_info st_other st_shndx ->
           Fields.create ~st_name ~st_value ~st_size ~st_info ~st_other ~st_shndx)
  end

  let value  = "value"   %: int (** not a good place for this *)
  let sh_link = "sh-link" %: int (** not a good place for this *)
  let sh_info = "sh-info" %: int (** not a good place for this *)
  let sh_entsize = "sh-entsize" %: int (** not a good place for this *)
  let is_fun = "is-function" %: bool (** will be deleted  *)

  let program_header () =
    declare "program-header"
      (scheme
         e_type $ p_off $ v_addr $ p_addr $ p_size $ v_size
       $ flags $ align)
      (fun p_type p_offset p_vaddr p_paddr p_filesz p_memsz p_flags p_align ->
         Phdr.Fields.create ~p_type ~p_offset ~p_vaddr ~p_paddr ~p_filesz ~p_memsz ~p_flags ~p_align)

  let section_header () =
    declare "section-header"
      (scheme index $ e_type $ flags $ v_addr $ p_off
       $ p_size $ sh_link $ sh_info $ align $ sh_entsize)
      (fun sh_name sh_type sh_flags sh_addr sh_offset sh_size sh_link
        sh_info sh_addralign sh_entsize ->
        Shdr.Fields.create
          ~sh_name ~sh_type ~sh_flags ~sh_addr ~sh_offset ~sh_size ~sh_link
          ~sh_info ~sh_addralign ~sh_entsize)


  let shstrtab_entry () =
    declare "shstrtab-entry" (scheme index $ name) (fun index name -> index,name)

  let strtab_entry () =
    declare "strtab-entry" (scheme index $ name) (fun index name -> index,name)

  let symbol_entry () = declare "symbol-entry"
      (scheme value $ p_size $ is_fun $ name)
      (fun addr size is_fun name -> addr, size, is_fun, name)

end

module Image_ogre_of_elf = struct
  open Bap_image.Scheme
  open Elf_scheme
  open Fact.Syntax

  module Strtab = Int64.Map

  let is flag expected = Int64.(bit_and flag (of_int expected) = one)

  let base_strtab tab_name =
    Fact.foreach Ogre.Query.(begin
        select (from Shdr.string_table)
          ~where:(Shdr.string_table.(Shdr.sec_name) = str(".shstrtab"))
      end)
      ~f:(fun (_,i,n) -> i,n) >>= fun s ->
    Fact.return (Strtab.of_alist_exn (Sequence.to_list s))

  let shstrtab = base_strtab shstrtab_entry
  let strtab = base_strtab strtab_entry

  let segments =
    let is_r flags = is flags 4 in
    let is_w flags = is flags 2 in
    let is_x flags = is flags 1 in
    let is_loadable typ = is typ 1 in
    let name_of_number n = sprintf "%02d" n in
    Fact.foreach Ogre.Query.(select (from program_header))
      ~f:ident >>= fun s ->
    Fact.Seq.fold s ~init:0
      ~f:(fun n {Phdr.p_type; p_flags; p_vaddr; p_filesz; p_memsz; p_offset} ->
          let r,w,x = is_r p_flags, is_w p_flags, is_x p_flags in
          if is_loadable p_type then
            Fact.provide segment p_vaddr p_memsz r w x >>= fun () ->
            Fact.provide mapped p_vaddr p_filesz p_offset >>= fun () ->
            Fact.provide named_region p_vaddr p_memsz (name_of_number n) >>= fun () ->
            Fact.return (n + 1)
          else Fact.return (n + 1)) >>= fun _ -> Fact.return ()

  let sections =
    Fact.foreach Ogre.Query.(select (from section_header))
      ~f:ident >>= fun s ->
    shstrtab >>= fun tab ->
    Fact.Seq.iter s
      ~f:(fun {Shdr.sh_name; sh_addr; sh_offset; sh_size} ->
          Fact.provide section sh_addr sh_size >>= fun () ->
          match Strtab.find tab sh_name with
          | None -> Fact.return ()
          | Some name ->
            Fact.provide named_region sh_addr sh_size name)

  let symbols =
    Fact.foreach Ogre.Query.(select (from symbol_entry))
      ~f:(fun (addr, size, is_fun, name) -> (addr, size, is_fun, name)) >>= fun s ->
    Fact.Seq.iter s ~f:(fun (addr, size, is_fun, name) ->
        Fact.provide named_symbol addr name >>= fun () ->
        Fact.provide symbol_chunk addr size addr >>= fun () ->
        if is_fun then
          Fact.provide code_start addr
        else Fact.return ())

  let img =
    segments >>= fun () ->
    sections >>= fun () ->
    symbols
end

let to_image_doc doc =
  printf "to image!\n";
  match Fact.exec Image_ogre_of_elf.img doc with
  | Ok doc ->
    printf "has doc!\n";
    (* printf "%s\n" @@ Ogre.Doc.to_string doc; *)
    Ok (Some doc)
  | Error er -> Error er

module Loader = struct
  let _from_data data =
    Bap_llvm_binary.bap_llvm_load data |>
    Ogre.Doc.from_string >>= fun doc -> to_image_doc doc

  let from_data data =
    let str = Bap_llvm_binary.bap_llvm_load data  in
    (* printf "%s\n" str; *)
    Ogre.Doc.from_string str >>= fun doc -> to_image_doc doc

  let from_file path = Bap_fileutils.readfile path |> from_data
end
