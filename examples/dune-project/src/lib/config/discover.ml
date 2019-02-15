module C = Configurator.V1
let () =
  C.main ~name:"glib" (fun c ->
    let default = { C.Pkg_config.libs = ["-lglib-2.0"]; cflags = [] } in
    let conf = match C.Pkg_config.get c with
    | None -> default
    | Some pc ->
      match C.Pkg_config.query pc ~package:"glib-2.0" with
      | None -> default
      | Some deps -> deps in

    (* this file is used by dune for compiling c cstubs *)
    C.Flags.write_sexp "c_flags.sexp" conf.cflags;

    (* this file is used by dune during linking *)
    C.Flags.write_sexp "c_library_flags.sexp" conf.libs;

    (* ppx_cstubs only needs $CFLAGS, it doesn't link anything *)
    C.Flags.write_lines "c_flags.lines" conf.cflags;
  )
