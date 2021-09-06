module List = CCListLabels

let remove_file f = try Sys.remove f with Sys_error _ -> ()

let compile ?stdout ~stderr c_prog f =
  let ocaml_flags = !Options.ocaml_flags in
  let dir =
    match !Options.ml_input_file with
    | None -> failwith "ml_input_file not set"
    | Some s -> Filename.dirname s
  in
  let pre_suf =
    match Std.Util.unsuffixed_file_name () with "" -> "" | x -> x ^ "_"
  in
  let pre = "ppxc_extract_" ^ pre_suf in
  let cfln = Filename.temp_file pre ".c" in
  Std.finally ~h:(fun () -> if not !Options.keep_tmp then remove_file cfln)
  @@ fun () ->
  CCIO.with_out ?mode:None ~flags:[ Open_creat; Open_trunc; Open_binary ] cfln
    (fun ch -> output_string ch c_prog);
  let obj = Filename.chop_suffix cfln ".c" ^ Ocaml_config.ext_obj () in
  let c_flags =
    (* that's a suboptimal solution. `ocamlc -c foo.c -o foo.o` doesn't work:
       "Options -c and -o are incompatible when compiling C files" But I might
       have no write access in the current directory and I'm unsure how '-I'
       flags and similar options are affected, if I change the current working
       directory ... *)
    match Ocaml_config.system () |> CCString.lowercase_ascii with
    | "win32" | "win64" -> [ "-Fo:" ^ obj ]
    | _ -> [ "-o"; obj ]
  in
  let c_flags = "-I" :: dir :: c_flags in
  let c_flags = !Options.c_flags @ c_flags in
  let args = List.map c_flags ~f:(fun c -> [ "-ccopt"; c ]) |> List.flatten in
  let args' =
    List.map !Options.findlib_pkgs ~f:(fun c -> [ "-package"; c ])
    |> List.flatten
  in
  let args = args' @ args in
  let args = if !Options.verbosity > 2 then args @ [ "-verbose" ] else args in
  let args = ocaml_flags @ args in
  let args =
    match Std.Various.use_threads () with
    | false -> args
    | true -> "-thread" :: args
    (* just to make ocamlfind silent. *)
  in
  let args =
    match !Options.cc with None -> args | Some s -> "-cc" :: s :: args
  in
  let args = "c" :: "-c" :: cfln :: args in
  let args =
    match !Options.toolchain with
    | None -> args
    | Some s -> "-toolchain" :: s :: args
  in
  let stdout =
    match stdout with
    | Some x -> x
    | None -> if !Options.verbosity > 0 then `Stdout else `Null
  in
  let prog = Options.ocamlfind in
  Std.finally ~h:(fun () -> if not !Options.keep_tmp then remove_file obj)
  @@ fun () ->
  if !Options.verbosity > 1 then Run.cmd_to_string prog args |> prerr_endline;
  match Run.run prog args ~stdout ~stderr with
  | exception Unix.Unix_error (e, s, _) ->
    let cmd = Run.cmd_to_string prog args in
    Std.Util.error "Process creation \"%s\" failed with %s (%S)" cmd
      (Unix.error_message e) s
  | n -> f n obj
