let () =
  let ocaml_where = Sys.argv.(1) in
  let output = Sys.argv.(2) in
  let ocaml_version = Sys.argv.(3) in
  let old_caml =
    Scanf.sscanf ocaml_version "%u.%u" (fun major minor ->
        (major, minor) < (4, 6))
  in
  let ch_out = open_out_bin output in
  output_char ch_out '(';
  Printf.fprintf ch_out "%S" "-linkall";
  ( if old_caml then
    let pre = "BYTECCLINKOPTS=" in
    let makefile = Filename.concat ocaml_where "Makefile.config" in
    CCIO.with_in makefile @@ fun ch ->
    let rec iter () =
      match CCIO.read_line ch with
      | None -> ()
      | Some s ->
        if CCString.prefix ~pre s = false then iter ()
        else
          let len_pre = String.length pre in
          let s = String.sub s len_pre (String.length s - len_pre) in
          let s = String.trim s in
          if s <> "" then
            CCString.split_on_char ' ' s
            |> List.iter (fun x -> Printf.fprintf ch_out " %S %S" "-cclib" x)
    in
    iter () );
  output_char ch_out ')';
  close_out ch_out
