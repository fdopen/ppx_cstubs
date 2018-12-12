(* This file is part of ppx_cstubs (https://github.com/fdopen/ppx_cstubs)
 * Copyright (c) 2018 fdopen
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>. *)

open Migrate_parsetree.Ast_405
module List = CCListLabels
type id = Marshal_types.id

module G = Script_result

module U = struct
  include Std.Util

  let from_id_loc_param x : Marshal_types.id_loc_param =
    Marshal.from_string x 0

  let unmarshal_expr s : Marshal_types.expr =
    Marshal.from_string s 0

end

module C_content = struct

  type t =
    | Header of string
    | Extract_text of string
    | Replace_text of id

  let default_header = {|#include <ctypes_cstubs_internals.h>|}
  let htl_id_text = Hashtbl.create 16

  let entries = ref [Header default_header]
  let clear () =
    entries := [Header default_header];
    Hashtbl.clear htl_id_text

  let update_extract_source = ref (fun _ -> ())

  let add_header h =
    entries := (Header h) :: !entries;
    !update_extract_source h

  let add_extract_source h =
    entries := (Extract_text h) :: !entries;
    !update_extract_source h

  let add_function id s = Hashtbl.replace htl_id_text id s
  let add_function_place id = entries := (Replace_text id) :: !entries

  let get_extract_source () =
    List.fold_left ~init:[] !entries ~f:(fun a -> function
      | Header h | Extract_text h  -> h::a
      | Replace_text _ -> a)
    |> String.concat "\n"

  let get_source () =
    let cnt,l =
      List.fold_left ~init:(0,[]) !entries ~f:(fun ((cnt,ac) as cnt_ac) ->
        function
        | Replace_text id ->
          let s = try Hashtbl.find htl_id_text id with
          | Not_found -> U.error "fatal: incomplete source code" in
          (succ cnt,(s::ac))
        | Header s -> cnt,(s::ac)
        | Extract_text _ -> cnt_ac) in
    if Hashtbl.length htl_id_text <> cnt then
      U.error "fatal: source code incomplete";
    if cnt = 0 then None
    else Some (String.concat "\n\n" l)
end


module Const = struct

  let extract_source = Buffer.create 256
  let htl_id_entries = Hashtbl.create 32
  let obj_code = ref None
  let htl_id_str_result = Hashtbl.create 32

  let clear () =
    Buffer.reset extract_source;
    Hashtbl.clear htl_id_entries;
    Hashtbl.clear htl_id_str_result;
    obj_code := None

  let init_buffer c_header =
    if Buffer.length extract_source = 0 then (
      Buffer.add_string extract_source Extract_c.prologue;
      Buffer.add_char extract_source '\n';
      Buffer.add_string extract_source c_header )

  let add id loc t str ~info_str =
    assert (!obj_code = None);
    let einfo = match Extract_c_ml.prepare t with
    | None ->
      (* TODO: fixme. Special case for strings? *)
      if Obj.magic t == Obj.magic Ctypes.string then
        `String
      else
        U.error "can't extract constants of type %s" (Ctypes.string_of_typ t)
    | Some i -> `Int i in
    let c_header = C_content.get_extract_source () in
    init_buffer c_header;
    let rinfo = match einfo with
    | `Int einf ->
      let open Extract_c_ml in
      let {min;max;signed;_} = einf in
      Extract_c.prepare_extract_int
        ~buf:extract_source ~c_header ?min ?max ~expr:str ~signed ()
    | `String ->
      Extract_c.prepare_extract_string
        ~buf:extract_source ~c_header ~expr:str () in
    Hashtbl.add htl_id_entries id (loc,einfo,rinfo,info_str)

  let update_extract_source header =
    assert (!obj_code = None);
    if Buffer.length extract_source < 1 then ()
    else
    let () = Buffer.add_char extract_source '\n' in
    Buffer.add_string extract_source header

  let find_failing () =
    let ebuf = Buffer.create 2048 in
    Std.with_return @@ fun r ->
    CCHashtbl.to_list htl_id_entries
    |> List.sort ~cmp:(fun (a,_) (b,_) -> compare a b)
    |> List.iter ~f:(fun (_id,(loc,_einfo,rinfo,info_str)) ->
      U.with_loc loc @@ fun () ->
      Buffer.clear ebuf;
      match Extract_c.compile ~ebuf rinfo.Extract_c.single_prog with
      | Error _ ->
        if Buffer.length ebuf > 0 then
          if !Options.verbosity > 0 then
            Buffer.output_buffer stderr ebuf
          else
          if !Options.verbosity = 0 then
            Buffer.contents ebuf
            |> CCString.split_on_char '\n'
            |> List.take 7
            |> List.map ~f:String.trim
            |> List.iter ~f:prerr_endline;
        r.Std.return (Some (loc,info_str))
      | Ok _ -> ());
    None

  let extract_common id (loc,einfo,rinfo,_) =
    if Hashtbl.mem G.htl_expr id then ()
    else
    U.with_loc loc @@ fun () ->
    let s =
      if !Options.mode = Options.Emulate then "1"
      else
      let obj = match !obj_code with
      | Some s -> s
      | None ->
        let s = Buffer.contents extract_source in
        match Extract_c.compile s with
        | Error s ->
          (match find_failing () with
          | Some (loc,s') -> U.error ~loc "%s ; couldn't extract %s" s s'
          | None -> U.error "%s" s);
        | Ok o ->
          obj_code := Some o;
          o in
      match Extract_c.extract rinfo obj with
      | Ok s -> s
      | Error x ->
        Extract_c.(match x with
        | Info_not_found ->
          U.error "constant extraction failed for unknown reasons"
        | User_overflow | Overflow -> U.error "overflow detected"
        | Underflow | User_underflow -> U.error "number too small"
        | Not_an_integer -> U.error "constant is not an integer") in
    (match einfo with
    | `Int einfo ->
      (match Extract_c_ml.gen einfo s with
      | None -> U.error "number not representable: %s" s
      | Some x -> Hashtbl.replace G.htl_expr id x)
    | `String ->
      U.str_expr s |> Hashtbl.replace G.htl_expr id);
    Hashtbl.replace htl_id_str_result  id s

  let extract_all () =
    Hashtbl.iter extract_common htl_id_entries

  let extract_single id =
    let x = match Hashtbl.find htl_id_entries id with
    | exception Not_found -> U.error "fatal: constant not found in hash table"
    | x -> x in
    extract_common id x

  let () = C_content.update_extract_source := update_extract_source
end


module Trace = struct

  type typ
  type fn

  let typs_struct : (typ * int) list ref = ref []
  let typs_union : (typ * int) list ref = ref []
  let typs_view : (typ * int) list ref = ref []
  let typs_pointer : (typ * int) list ref = ref []
  let typs_funptr : (typ * int) list ref = ref []
  let typs_array : (typ * int) list ref = ref []
  let typs_bigarray : (typ * int) list ref = ref []
  let typs_ocaml : (typ * int) list ref = ref []
  let typs_prim : (typ * int) list ref = ref []
  let typs_void : (typ * int) list ref = ref []
  let typs_abstract : (typ * int) list ref = ref []

  let fns : (fn * int) list ref = ref []

  let clear () =
    fns := [];
    typs_struct := [];
    typs_union := [];
    typs_view := [];
    typs_pointer := [];
    typs_funptr := [];
    typs_array := [];
    typs_bigarray := [];
    typs_ocaml := [];
    typs_prim := [];
    typs_void := [];
    typs_abstract := []

  let register_type : type a. int -> a Ctypes.typ -> unit =
    let open Ctypes_static in
    fun id t ->
      let x = match t with
      | Struct _ -> typs_struct
      | Union _ -> typs_union
      | View _ -> typs_view
      | Pointer _ -> typs_pointer
      | Funptr _ -> typs_funptr
      | Array _ -> typs_array
      | Bigarray _ -> typs_bigarray
      | OCaml _ -> typs_ocaml
      | Primitive _ -> typs_prim
      | Void -> typs_void
      | Abstract _ -> typs_abstract in
      let t = Obj.magic t in
      x := (t,id) :: !x

  let register_fn id (t:'a Ctypes.fn) =
    let t = Obj.magic t in
    fns := (t,id) :: !fns

  let rec trace : type a. a Ctypes.typ -> unit =
    let open Ctypes_static in
    fun cur ->
      let h l =
        let cur = Obj.magic cur in
        let r = List.find_map l
            ~f:(fun (x,id) -> if x == cur then Some id else None) in
        match r with
        | None -> ()
        | Some id ->
          if Hashtbl.mem G.htl_used id = false then
            Hashtbl.replace G.htl_used id ()
      in
      match cur with
      | Struct _ -> h !typs_struct
      | Union _ -> h !typs_union
      | View { ty; _ } -> h !typs_view; trace ty
      | Pointer ty ->
        h !typs_pointer; trace ty
      | Funptr fn ->
        h !typs_funptr; trace_fn fn
      | Array _ -> h !typs_array
      | Bigarray _ -> h !typs_bigarray
      | OCaml _ -> h !typs_ocaml
      | Primitive _ -> h !typs_prim
      | Void -> h !typs_void
      | Abstract _ -> h !typs_abstract
  and trace_fn : type b. b Ctypes.fn -> unit =
    let open Ctypes_static in
    fun cur ->
      let t = Obj.magic cur in
      let r = List.find_map !fns
          ~f:(fun (x,id) -> if x == t then Some id else None) in
      (match r with
      | None -> ()
      | Some id ->
        if Hashtbl.mem G.htl_used id = false then
          Hashtbl.replace G.htl_used id ());
      match cur with
      | Returns a -> trace a
      | Function (a, b) ->
        trace a;
        trace_fn b
end

module Extract = struct

  let constant id_loc str t =
    let id,loc = U.from_id_loc_param id_loc in
    U.with_loc loc @@ fun () ->
    let info_str = "constant " ^ str in
    Const.add ~info_str id loc t str

  let register_fun_place id_loc_external =
    let id_external,_ = U.from_id_loc_param id_loc_external in
    C_content.add_function_place id_external

  let header (s:string) = C_content.add_header s

  let field id_loc stru field_name =
    let id,loc = U.from_id_loc_param id_loc in
    let ctyp = Gen_c.string_of_typ_exn stru in
    let to_extract = Printf.sprintf "offsetof(%s,%s)" ctyp field_name in
    Const.add
      ~info_str:(Printf.sprintf "offset of %s in %s" ctyp field_name)
      id loc Ctypes.camlint to_extract

  let seal id_loc_size id_loc_align stru =
    let id_size,loc_size = U.from_id_loc_param id_loc_size in
    let id_align,loc_align = U.from_id_loc_param id_loc_align in
    let ctyp = Gen_c.string_of_typ_exn stru in
    let size_str = Printf.sprintf "sizeof(%s)" ctyp in
    let struct_name = U.safe_cname ~prefix:"extract_struct" in
    let dummy_header =
      Printf.sprintf "struct %s {char c; %s x;};\n" struct_name ctyp in
    C_content.add_extract_source dummy_header;
    let align_str = Printf.sprintf "offsetof(struct %s,x)" struct_name in
    Const.add ~info_str:("size of " ^ ctyp)
      id_size loc_size Ctypes.camlint size_str;
    Const.add ~info_str:("align of " ^ ctyp)
      id_align loc_align Ctypes.camlint align_str

  let enum id_loc ?(typedef=false) name =
    let id,loc = U.from_id_loc_param id_loc in
    let info_str = "enum_type " ^ name in
    let str = match typedef with
    | false -> String.concat " " ["enum"; name]
    | true -> name in
    let str = "PPXC_CTYPES_ARITHMETIC_TYPEINFO(" ^ str ^ ")" in
    Const.add ~info_str id loc Ctypes.camlint str;
    (* wrong Ctypes.typ, wrong arithmetic type - but it's
       not used here to build externals *)
    Cstubs_internals.build_enum_type
      name
      Ctypes_static.Int32
      ~typedef
      [(`A,1L)]

  let get_enum_id = function
  | Marshal_types.E_normal(x)
  | Marshal_types.E_bitmask(x)
  | Marshal_types.E_normal_bitmask(x,_) -> x

  let enum_fake_view (l : 'a list) (typedef:bool) (name:string)
      (typ: 'b Ctypes.typ) : 'a Ctypes.typ * 'a list Ctypes.typ =
    let () = ignore (l : 'a list) in (* just to type the function *)
    let typedef = if typedef then "" else "enum " in
    let format_typ k fmt = Format.fprintf fmt "%s%s%t" typedef name k in
    let write _ = assert false in
    let read _ = assert false in
    Ctypes.view ~read ~write ~format_typ typ,
    Ctypes.view ~read ~write ~format_typ typ

  let enum2 (l : 'a list) (p:string) : 'a Ctypes.typ * 'a list Ctypes.typ =
    let open Marshal_types in
    let {enum_l; enum_name; enum_is_typedef;
         enum_type_id; enum_loc; enum_unexpected = _ } = Marshal.from_string p 0 in
    U.with_loc enum_loc @@ fun () ->
    let info_str = "enum_type " ^ enum_name in
    let str = match enum_is_typedef with
    | false -> String.concat " " ["enum"; enum_name]
    | true -> enum_name in
    let str = "PPXC_CTYPES_ARITHMETIC_TYPEINFO(" ^ str ^ ")" in
    Const.add ~info_str (get_enum_id enum_type_id) enum_loc Ctypes.camlint str;
    ListLabels.iter enum_l ~f:(fun c ->
      U.with_loc c.ee_loc @@ fun () ->
      let info_str = "enum constant " ^ c.ee_cname in
      Const.add ~info_str c.ee_signed_id c.ee_loc Ctypes.int64_t c.ee_cname;
      Const.add ~info_str c.ee_unsigned_id c.ee_loc Ctypes.uint64_t c.ee_cname);
    (* wrong size, but not used inside extraction script *)
    enum_fake_view l enum_is_typedef enum_name Ctypes.int32_t

end

module Build = struct


  let reg_trace_fn id t =
    Trace.register_fn id t;
    t

  let reg_trace id t =
    Trace.register_type id t;
    t

  let constant =
    let open Ctypes_static in
    fun id_loc str ->
      let id,_loc = U.from_id_loc_param id_loc in
      Const.extract_single id;
      let res =
        try
          Hashtbl.find Const.htl_id_str_result id
        with
        | Not_found -> U.error "couldn't extract constant %S" str in
      let ef t =
        U.error "constants of type %s not possible"
          (Ctypes.string_of_typ t) in
      let int () = int_of_string res in
      let f : type a. a Ctypes.typ -> a =
        fun t ->
          match t with
          | Void -> ef t
          | Struct _ -> ef t
          | Union _ -> ef t
          | Array _ -> ef t
          | Bigarray _ -> ef t
          | Abstract _ -> ef t
          | Pointer _  -> ef t
          | Funptr _ -> ef t
          | OCaml _ -> ef t
          | View { ty = Pointer (Primitive (Ctypes_primitive_types.Char));
                   read ; _ } ->
            let len = String.length res in
            let ar = Ctypes.CArray.make Ctypes.char (len + 1) in
            String.iteri (Ctypes.CArray.set ar) res;
            Ctypes.CArray.set ar len '\x00';
            read (Ctypes.CArray.start ar)
          | View _ -> ef t
          | Primitive p ->
            let module Cp = Ctypes_primitive_types in
            match p with
            | Cp.Schar -> int ()
            | Cp.Char -> int_of_string res |> Char.chr
            | Cp.Uchar -> Unsigned.UChar.of_string res
            | Cp.Bool -> int_of_string res <> 0
            | Cp.Short -> int ()
            | Cp.Int -> int ()
            | Cp.Long -> Signed.Long.of_string res
            | Cp.Llong -> Signed.LLong.of_string res
            | Cp.Ushort -> Unsigned.UShort.of_string res
            | Cp.Sint -> Signed.SInt.of_string res
            | Cp.Uint -> Unsigned.UInt.of_string res
            | Cp.Ulong -> Unsigned.ULong.of_string res
            | Cp.Ullong -> Unsigned.ULLong.of_string res
            | Cp.Size_t -> Unsigned.Size_t.of_string res
            | Cp.Int8_t -> int ()
            | Cp.Int16_t -> int ()
            | Cp.Int32_t -> Int32.of_string res
            | Cp.Int64_t -> Int64.of_string res
            | Cp.Uint8_t -> Unsigned.UInt8.of_string res
            | Cp.Uint16_t -> Unsigned.UInt16.of_string res
            | Cp.Uint32_t -> Unsigned.UInt32.of_string res
            | Cp.Uint64_t -> Unsigned.UInt64.of_string res
            | Cp.Camlint -> int ()
            | Cp.Nativeint -> Nativeint.of_string res
            | Cp.Float -> ef t
            | Cp.Double -> ef t
            | Cp.LDouble -> ef t
            | Cp.Complex32 -> ef t
            | Cp.Complex64 -> ef t
            | Cp.Complexld -> ef t
      in
      f

  let foreign_value id_loc_external id_loc_stri_expr
      ctypes_t foreign_value name typ_expr =
    let id_external,_loc_external = U.from_id_loc_param id_loc_external in
    let id_stri_expr,_loc_stri_expr = U.from_id_loc_param id_loc_stri_expr in
    let typ_expr = U.unmarshal_expr typ_expr in
    Trace.trace ctypes_t;
    let fun' = Ctypes.(void @-> returning (ptr ctypes_t)) in
    let ptrexpr = [%expr Ctypes.ptr [%e typ_expr]] in
    let stubname = U.safe_cname ~prefix:("value_" ^ foreign_value) in
    let cinfo = Gen_c.gen_value fun' ~stubname ~value:foreign_value in
    C_content.add_function id_external cinfo.Gen_c.stub_source;
    let res = Gen_ml.foreign_value fun' name ptrexpr cinfo in
    Hashtbl.replace G.htl_stri id_external res.Gen_ml.extern;
    Hashtbl.replace G.htl_stri id_stri_expr res.Gen_ml.intern

  let external' id_loc_external id_loc_stri_expr ctypes_fn ~marshal_info =
    let id_external,_loc_external = U.from_id_loc_param id_loc_external in
    let id_stri_expr,_loc_stri_expr = U.from_id_loc_param id_loc_stri_expr in
    let open Marshal_types in
    let {el; ret; release_runtime_lock; noalloc; is_inline; remove_labels;
         return_errno; c_name; ocaml_name; prim_name} =
      Marshal.from_string marshal_info 0 in
    let c =
      if is_inline = false then
        let stubname = U.safe_cname ~prefix:c_name in
        Gen_c.gen_fun ctypes_fn ~stubname ~cfunc:c_name
          ~release_runtime_lock ~noalloc ~return_errno
      else
      let names =
        if List.exists el ~f:(fun (x,_) -> x = Asttypes.Nolabel) then
          let is_void : type a. a Ctypes.fn -> bool = function
          | Ctypes_static.Function(a,_) -> Gen_c.is_void a
          | Ctypes_static.Returns _ ->  false in
          if List.length el = 1 && is_void ctypes_fn then
            ["dummy"]
          else
            U.error "inline code requires named parameters"
        else
        let names = ListLabels.map el ~f:Asttypes.(fun (x,_) -> match x with
        | Nolabel | Optional _ ->  U.error "label required"
        | Labelled s -> s) in
        let names' = CCList.uniq ~eq:CCString.equal names in
        if List.length names' <> List.length names then
          U.error "labels must be unique";
        names' in
      let names =
        ListLabels.mapi names ~f:(fun i a ->
          let s = Printf.sprintf "ppxc__var%d_%s" i (U.safe_ascii_only a) in
          a,s) in
      let c_body = c_name in
      let prim_name = U.safe_ascii_only prim_name in
      let c_name = U.safe_cname ~prefix:("i" ^ prim_name) in
      let body =
        Gen_c.build_inline_fun ctypes_fn ~c_name ~c_body ~noalloc names in
      C_content.add_function id_stri_expr body;
      let stubname = U.safe_cname ~prefix:("f" ^ prim_name) in
      Gen_c.gen_fun ctypes_fn ~stubname ~cfunc:c_name ~release_runtime_lock
        ~noalloc ~return_errno in
    C_content.add_function id_external c.Gen_c.stub_source;
    let el =
      if is_inline = false then
        el
      else if remove_labels then
        List.map el ~f:(fun (_,b) -> Asttypes.Nolabel,b)
      else
        ListLabels.map el ~f:(fun ((x,y) as xy) ->
          let open Asttypes in
          match x with
          | Labelled(s) ->
            let l = String.length s in
            if l > 0 && s.[l-1] = '_' then
              (Nolabel,y)
            else
              xy
          | Nolabel | Optional _ -> xy) in
    let res = Gen_ml.external' ctypes_fn ocaml_name el ret c in
    Hashtbl.replace G.htl_stri id_external res.Gen_ml.extern;
    Hashtbl.replace G.htl_stri id_stri_expr res.Gen_ml.intern

  let enum_size_unsigned id name =
    Const.extract_single id;
    let res =
      try
        let s = Hashtbl.find Const.htl_id_str_result id in
        int_of_string s
      with
      | Not_found
      | Failure _ -> U.error "can't determine type of enum %s" name in
    let float_flag_bit = 15 (* constants from ctypes_primitives.h *)
    and unsigned_flag_bit = 14 in
    let is_float = res land (1 lsl float_flag_bit) <> 0 in
    let is_unsigned = res land (1 lsl unsigned_flag_bit) <> 0 in
    let size = res land (lnot (1 lsl float_flag_bit)) in
    let size = size land (lnot (1 lsl unsigned_flag_bit)) in
    if is_float then
      U.error "Enum type detected as floating type: %s" name;
    size,is_unsigned

  let enum id_loc name ?(typedef=false) ?unexpected l =
    let id,_loc = U.from_id_loc_param id_loc in
    let a_type,a_type_expr = match enum_size_unsigned id name with
    | 1, false -> Ctypes_static.Int8, [%expr Ctypes_static.Int8]
    | 2, false -> Ctypes_static.Int16, [%expr Ctypes_static.Int16]
    | 4, false -> Ctypes_static.Int32, [%expr Ctypes_static.Int32]
    | 8, false -> Ctypes_static.Int64, [%expr Ctypes_static.Int64]
    | 1, true -> Ctypes_static.Uint8, [%expr Ctypes_static.Uint8]
    | 2, true -> Ctypes_static.Uint16, [%expr Ctypes_static.Uint16]
    | 4, true -> Ctypes_static.Uint32, [%expr Ctypes_static.Uint32]
    | 8, true -> Ctypes_static.Uint64, [%expr Ctypes_static.Uint64]
    | x, _ -> U.error "enum %s has unsupported size of %d" name x in
    Hashtbl.replace G.htl_expr id a_type_expr;
    Cstubs_internals.build_enum_type name a_type ~typedef ?unexpected l

  let enum2 (l : 'a list) (p:string) : 'a Ctypes.typ * 'a list Ctypes.typ =
    let open Marshal_types in
    let {enum_l; enum_name; enum_is_typedef; enum_type_id;
         enum_unexpected ; enum_loc = _ } = Marshal.from_string p 0 in
    let type_size, type_unsigned =
      enum_size_unsigned (Extract.get_enum_id enum_type_id) enum_name in
    let unboxed_ints =
      type_unsigned = false &&
      Ocaml_config.word_size () = 64 &&
      !Options.toolchain = None &&
      Ctypes.alignment Ctypes.int32_t = Ctypes.alignment Ctypes.int &&
      Ctypes.sizeof Ctypes.int32_t = Ctypes.sizeof Ctypes.int in
    let typ,typ_expr,live_res =
      let def typ expr =
        let a = match Extract_c_ml.prepare typ with
        | None -> assert false
        | Some x -> x in
        a, expr, Extract.enum_fake_view l enum_is_typedef enum_name typ in
      match type_size, type_unsigned with
      | 1, false -> def Ctypes.int8_t [%expr Ctypes.int8_t]
      | 2, false -> def Ctypes.int16_t [%expr Ctypes.int16_t]
      | 4, false ->
        if unboxed_ints then
          def Ctypes.int [%expr Ctypes.int]
        else
          def Ctypes.int32_t [%expr Ctypes.int32_t]
      | 8, false -> def Ctypes.int64_t [%expr Ctypes.int64_t]
      | 1, true -> def Ctypes.uint8_t [%expr Ctypes.uint8_t]
      | 2, true -> def Ctypes.uint16_t [%expr Ctypes.uint16_t]
      | 4, true -> def Ctypes.uint32_t [%expr Ctypes.uint32_t]
      | 8, true -> def Ctypes.uint64_t [%expr Ctypes.uint64_t]
      | x, _ -> U.error "enum %s has unsupported size of %d" enum_name x in
    let additional_check =
      if unboxed_ints && type_size = 4 then
        Extract_c_ml.prepare Ctypes.int32_t
      else
        None in
    let list =
      ListLabels.fold_right ~init:([%expr []]) enum_l
        ~f:(fun c ac ->
          U.with_loc c.ee_loc @@ fun () ->
          let id = match type_unsigned with
          | true -> c.ee_unsigned_id
          | false -> c.ee_signed_id in
          Const.extract_single id;
          let i_str =
            try Hashtbl.find Const.htl_id_str_result id
            with Not_found ->
              U.error "can't determine enum constant %S" c.ee_cname in
          (* avoid overflow detection in Extract.extract_all *)
          Hashtbl.remove Const.htl_id_entries c.ee_signed_id;
          Hashtbl.remove Const.htl_id_entries c.ee_unsigned_id;
          let f typ = match  Extract_c_ml.gen typ i_str with
          | None ->
            U.error "enum constant %S is not of type %S" c.ee_cname enum_name
          | Some s -> s in
          let integer = f typ in
          (match additional_check with
          | None -> ()
          | Some t -> ignore ( f t : Marshal_types.expr));
          let tup = Ast_helper.Exp.tuple [c.ee_expr; integer] in
          let tup = Ast_helper.Exp.tuple [tup; ac] in
          Ast_helper.Exp.construct (U.mk_lid "::") (Some tup)) in
    let name = Std.Util.str_expr enum_name in
    let typedef = match enum_is_typedef with
    | true -> [%expr true]
    | false -> [%expr false] in
    (match enum_type_id with
    | Marshal_types.E_normal(id) | Marshal_types.E_normal_bitmask(id,_) ->
      let e = [%expr
        Ppx_cstubs_internals.build_enum
          [%e name]
          [%e typ_expr]
          ~typedef:[%e typedef]
          ?unexpected:[%e enum_unexpected]
          [%e list] ] in
      Hashtbl.replace G.htl_expr id e
    | Marshal_types.E_bitmask _ -> ());
    (match enum_type_id with
    | Marshal_types.E_bitmask(id) | Marshal_types.E_normal_bitmask(_,id) ->
      let e = [%expr
        Ppx_cstubs_internals.build_enum_bitmask
          [%e name]
          [%e typ_expr]
          ~typedef:[%e typedef]
          [%e list] ] in
      Hashtbl.replace G.htl_expr id e
    | Marshal_types.E_normal _ -> ());
    live_res

  let foreign id_loc_external id_loc_stri_expr ~ocaml_name ~typ_expr
      ?(release_runtime_lock=false) ?(noalloc=false) ?(return_errno=false)
      c_name fn =
    let id_external,_loc_external = U.from_id_loc_param id_loc_external in
    let id_stri_expr,_loc_stri_expr = U.from_id_loc_param id_loc_stri_expr in
    let stubname = U.safe_cname ~prefix:c_name in
    Trace.trace_fn fn;
    let c = Gen_c.gen_fun fn
        ~stubname ~cfunc:c_name ~release_runtime_lock ~noalloc ~return_errno in
    C_content.add_function id_external c.Gen_c.stub_source;
    let typ_expr = U.unmarshal_expr typ_expr in
    let res = Gen_ml.foreign fn ocaml_name c typ_expr in
    Hashtbl.replace G.htl_stri id_external res.Gen_ml.extern;
    Hashtbl.replace G.htl_stri id_stri_expr res.Gen_ml.intern

end

module Ctypes_make = struct

  module type Signed_type =
  sig
    include Signed.S
    val t : t Ctypes_static.typ
  end

  module type Unsigned_type =
  sig
    include Unsigned.S
    val t : t Ctypes_static.typ
  end

  module type Target_env =
  sig
    module Uintptr : Unsigned_type
    module Intptr : Signed_type
    module Ptrdiff : Signed_type
    (* FIXME: PosixTypes missing ... *)
  end

  module Info : Target_env = struct
    module Intptr : Signed_type = struct
      include Ctypes.Intptr
      let t = Ctypes.intptr_t
    end
    module Uintptr : Unsigned_type = struct
      include Ctypes.Uintptr
      let t = Ctypes.uintptr_t
    end
    module Ptrdiff : Signed_type = struct
      include Ctypes.Ptrdiff
      let t = Ctypes.ptrdiff_t
    end
  end

  module Info32 : Target_env = struct
    module Intptr : Signed_type = struct
      include Signed.Int32
      let t = Ctypes_static.(typedef int32_t "intptr_t")
    end
    module Uintptr : Unsigned_type = struct
      include Unsigned.UInt32
      let t = Ctypes_static.(typedef uint32_t "uintptr_t")
    end
    module Ptrdiff : Signed_type = struct
      include Signed.Int32
      let t = Ctypes_static.(typedef int32_t "ptrdiff_t")
    end
  end

  module Info64 : Target_env = struct
    module Intptr : Signed_type = struct
      include Signed.Int64
      let t = Ctypes_static.(typedef int64_t "intptr_t")
    end
    module Uintptr : Unsigned_type = struct
      include Unsigned.UInt64
      let t = Ctypes_static.(typedef uint64_t "uintptr_t")
    end
    module Ptrdiff : Signed_type = struct
      include Signed.Int64
      let t = Ctypes_static.(typedef int64_t "ptrdiff_t")
    end
  end

  module Ctypes (T_env:Target_env) = struct

    include Ctypes_static

    let ppxc__private_seal = Ctypes.seal
    let typ_of_bigarray_kind = Ctypes.typ_of_bigarray_kind
    let string_opt = Ctypes.string_opt
    let string = Ctypes.string

    let ppxc__private_make = Ctypes.make
    let ppxc__private_setf = Ctypes.setf
    let ppxc__private_getf = Ctypes.getf

    let ppxc__private_ocaml_typ typ =
      let s : [`priv] Ctypes_static.structure Ctypes.typ = Ctypes.structure typ in
      let _ : _ Ctypes.field = Ctypes.field s "i" Ctypes.camlint in
      let () = Ctypes.seal s in
      Ctypes.view ~format_typ:Evil_hack.format_typ
        ~read:Std.identity ~write:Std.identity s

    let genarray = Ctypes.genarray

    let array1 = Ctypes.array1
    let array2 = Ctypes.array2
    let array3 = Ctypes.array3

    let static_funptr x =
      Trace.trace_fn x;
      static_funptr x

    let (@->) a b =
      Trace.trace a;
      Trace.trace_fn b;
      (@->) a b

    let returning a =
      Trace.trace a;
      returning a

    let typedef a b =
      Trace.trace a;
      typedef a b

    let view ?format_typ ?format ~read ~write a =
      Trace.trace a;
      view ?format_typ ?format ~read ~write a

    let array i a =
      Trace.trace a;
      array i a

    let ptr a =
      Trace.trace a;
      ptr a

    let ptr_opt a =
      Trace.trace a;
      Ctypes.ptr_opt a

    let ppxc__private_field a s t =
      (* Trace.trace a; *)
      Trace.trace t;
      Ctypes.field a s t

    module Intptr = T_env.Intptr
    module Uintptr = T_env.Uintptr
    module Ptrdiff = T_env.Ptrdiff
    let intptr_t = Intptr.t
    let uintptr_t = Uintptr.t
    let ptrdiff_t = Ptrdiff.t
  end
end

module Run_environment = struct

  module Libffi_abi = struct
    type abi = unit
    let aix = ()
    let darwin = ()
    let eabi = ()
    let fastcall = ()
    let gcc_sysv = ()
    let linux = ()
    let linux64 = ()
    let linux_soft_float = ()
    let ms_cdecl = ()
    let n32 = ()
    let n32_soft_float = ()
    let n64 = ()
    let n64_soft_float = ()
    let o32 = ()
    let o32_soft_float = ()
    let osf = ()
    let pa32 = ()
    let stdcall = ()
    let sysv = ()
    let thiscall = ()
    let unix = ()
    let unix64 = ()
    let v8 = ()
    let v8plus = ()
    let v9 = ()
    let vfp = ()
    let default_abi = ()
  end

  module Foreign = struct
    let funptr ?abi:_ ?name:_ ?check_errno:_ ?runtime_lock:_
        ?thread_registration:_ fn =
      G.foreign_used := true;
      let read _ = assert false
      and write _ = assert false in
      Trace.trace_fn fn;
      Ctypes_static.(view ~read ~write (static_funptr fn))

    let funptr_opt ?abi:_ ?name:_ ?check_errno:_ ?runtime_lock:_
        ?thread_registration:_ fn =
      G.foreign_used := true;
      Trace.trace_fn fn;
      let read _ = assert false
      and write _ = assert false in
      Ctypes_static.(view ~read ~write (static_funptr fn))
  end
  let funptr = Foreign.funptr
  let funptr_opt = Foreign.funptr_opt
  module Ctypes_static = struct end
  module Ctypes_primitive_types = struct end
  module Ctypes_printers = struct end
  module Ctypes_structs = struct end
  module Ctypes_types = struct end
end

module Main = struct

  let clear () =
    C_content.clear ();
    Trace.clear ();
    Const.clear ()

  let run () =
    Const.extract_all ();
    G.c_source := C_content.get_source ();
    clear ()

end

let set_loc loc =
  let loc : Marshal_types.loc = Marshal.from_string loc 0 in
  Ast_helper.default_loc := loc

(* This file is only used through the toplevel and not referenced
   directly. This way a dependency order is enforced *)
let _init () = ()
