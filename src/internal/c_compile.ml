module List = CCListLabels

let uniq l = List.uniq ~eq:CCString.equal l

let pkgs () = uniq ("ctypes" :: !Options.findlib_pkgs)

let get_include_dirs_cross () =
  let args = uniq ("ctypes" :: !Options.findlib_pkgs) in
  let args = "query" :: "-recursive" :: args in
  let args =
    match !Options.toolchain with
    | None -> args
    | Some s -> "-toolchain" :: s :: args
  in
  let buf = Buffer.create 128 in
  let stdout = `Buffer buf in
  let prog = Options.ocamlfind in
  if !Options.verbosity > 2 then Run.cmd_to_string prog args |> prerr_endline;
  (match Run.run prog args ~stdout with
  | exception Unix.Unix_error (e, s, _) ->
    let cmd = Run.cmd_to_string prog args in
    Std.Util.error "Process creation \"%s\" failed with %s (%S)" cmd
      (Unix.error_message e) s
  | 0 -> ()
  | ec -> Std.Util.error "`ocamlfind query` failed with %d" ec);
  let re = Re.Perl.re "[\n]+" |> Re.compile in
  let l = Buffer.contents buf |> Re.split re in
  Ocaml_config.standard_library () :: l

let get_include_dirs_live () =
  let pkgs = pkgs () in
  try
    let l =
      Findlib.package_deep_ancestors [ "bytes" ] pkgs
      |> List.map ~f:Findlib.package_directory
    in
    Findlib.ocaml_stdlib () :: l
  with Fl_package_base.No_such_package (s, s') ->
    if s' = "" then Std.Util.error "error: findlib package %s not found\n%!" s
    else Std.Util.error "error: findlib package %s (%S) not found\n%!" s s'

let get_include_dirs () =
  let l =
    if Options.toolchain_used () then get_include_dirs_cross ()
    else get_include_dirs_live ()
  in
  let dir =
    match !Options.ml_input_file with
    | None -> failwith "ml_input_file not set"
    | Some s -> Filename.dirname s
  in
  uniq (l @ !Options.ocaml_include_dirs @ [ dir ])

let remove_file f = try Sys.remove f with Sys_error _ -> ()

let compile ?stdout ~stderr c_prog f =
  let pre_suf =
    match Std.Util.unsuffixed_file_name () with "" -> "" | x -> x ^ "_"
  in
  let pre = "ppxc_extract_" ^ pre_suf in
  let idirs = get_include_dirs () in
  let idirs = List.map idirs ~f:(fun c -> [ "-I"; c ]) |> List.flatten in
  let default_cc, default_cflags = Ocaml_config.c_compiler_flags () in
  let use_cxx = !Options.use_cxx in
  let suf = if use_cxx then ".cpp" else ".c" in
  let msvc =
    match Ocaml_config.system () |> CCString.lowercase_ascii with
    | "win32" | "win64" -> true
    | _ -> false
  in
  let default_cflags =
    if use_cxx = false then default_cflags
    else
      let re = if msvc then "^(/|-)std:c[0-9]" else "^-std=(gnu|c)[0-9]" in
      let re = Re.Perl.re re |> Re.compile in
      List.filter ~f:(fun s -> Re.execp re s = false) default_cflags
  in
  let cfln = Filename.temp_file pre suf in
  Std.finally ~h:(fun () -> if not !Options.keep_tmp then remove_file cfln)
  @@ fun () ->
  CCIO.with_out ?mode:None ~flags:[ Open_creat; Open_trunc; Open_binary ] cfln
    (fun ch -> output_string ch c_prog);
  let obj = Filename.chop_suffix cfln suf ^ Ocaml_config.ext_obj () in
  let args = if msvc then [ "-Fo:" ^ obj ] else [ "-o"; obj ] in
  let args = "-c" :: cfln :: args in
  let args =
    if use_cxx = false || !Options.cc <> None then args
    else if msvc then "-TP" :: args
    else "-x" :: "c++" :: args
  in
  let args = default_cflags @ !Options.c_flags @ idirs @ args in
  let prog = match !Options.cc with None -> default_cc | Some s -> s in
  let stdout =
    match stdout with
    | Some x -> x
    | None -> if !Options.verbosity > 0 then `Stdout else `Null
  in
  Std.finally ~h:(fun () -> if not !Options.keep_tmp then remove_file obj)
  @@ fun () ->
  if !Options.verbosity > 1 then Run.cmd_to_string prog args |> prerr_endline;
  match Run.run prog args ~stdout ~stderr with
  | exception Unix.Unix_error (e, s, _) ->
    let cmd = Run.cmd_to_string prog args in
    Std.Util.error "Process creation \"%s\" failed with %s (%S)" cmd
      (Unix.error_message e) s
  | n -> f n obj
