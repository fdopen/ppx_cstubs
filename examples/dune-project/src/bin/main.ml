let () =
  let name =
    match Namelib.real_name () with None -> "unknown person" | Some s -> s
  in
  Printf.printf "Hello, %s!\n" name ;
  exit 0
