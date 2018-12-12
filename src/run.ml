type pipe_state =
| Open
| Closed
| Uninit

type pipe_with_status =
  {
    mutable state: pipe_state;
    mutable fd: Unix.file_descr
  }

let dev_null = if Sys.win32 then "NUL" else "/dev/null"

let pipe a b =
  let tmp1,tmp2 = Unix.pipe () in
  a.state <- Open;
  a.fd <- tmp1;
  b.state <- Open;
  b.fd <- tmp2

let new_pipe () =
  {state = Uninit;
   fd = Unix.stderr}

let rec eintr1 f a =
  try
    f a
  with
  | Unix.Unix_error(Unix.EINTR,_,_) ->  eintr1 f a

let rec eintr2 f a b =
  try
    f a b
  with
  | Unix.Unix_error(Unix.EINTR,_,_) -> eintr2 f a b

let rec eintr3 f a b c =
  try
    f a b c
  with
  | Unix.Unix_error(Unix.EINTR,_,_) -> eintr3 f a b c


let rec eintr4 f a b c d=
  try
    f a b c d
  with
  | Unix.Unix_error(Unix.EINTR,_,_) -> eintr4 f a b c d

let rec eintr6 f a b c d e g =
  try
    f a b c d e g
  with
  | Unix.Unix_error(Unix.EINTR,_,_) ->
    eintr6 f a b c d e g


let close_pipe a =
  match a.state with
  | Closed
  | Uninit -> ()
  | Open ->
    a.state <- Closed ;
    (* capturing EINTR Os specific ... *)
    Unix.close a.fd

type io_out = [
  | `Fd of Unix.file_descr
  | `Null
  | `Stdout
  | `Stderr
  | `Buffer of Buffer.t
  | `Fun of (string -> unit)]

type io_in =
  [`String of string
  |`Null
  |`Fd of Unix.file_descr
  ]

let close_pipe_ne a =
  try
    close_pipe a
  with
  | Unix.Unix_error _ -> ()

let str_buffer_len = 8192

let run ?(env=Unix.environment ())
    ?(stdin=`Null) ?(stderr=`Stderr) ?(stdout=`Stdout) prog args : int =
  let tmp_str = Bytes.create str_buffer_len
  and p_stdout_read = new_pipe ()
  and p_stdout_write = new_pipe ()
  and p_stderr_read = new_pipe ()
  and p_stderr_write = new_pipe ()
  and p_stdin_read = new_pipe ()
  and p_stdin_write = new_pipe ()
  and args = Array.of_list (prog::args) in
  Std.finally ~h:(fun () ->
    close_pipe_ne p_stdin_read;
    close_pipe_ne p_stdin_write;
    close_pipe_ne p_stdout_read;
    close_pipe_ne p_stdout_write;
    close_pipe_ne p_stderr_write;
    close_pipe_ne p_stderr_read )
  @@ fun () ->
  let () =
    let comm p fd =
      let fd = eintr1 (fun x -> Unix.dup x) fd in
      p.fd <-  fd;
      p.state <- Open
    in
    let out p_out_write p_out_read out =
      (match out with
      | `Stdout ->
        if Sys.win32 then
          p_out_write.fd <- Unix.stdout
        else
          comm p_out_write Unix.stdout
      | `Stderr ->
        if Sys.win32 then
          p_out_write.fd <- Unix.stderr
        else
          comm p_out_write Unix.stderr
      | `Null ->
        let fd = eintr3 Unix.openfile dev_null [ Unix.O_WRONLY ] 0o600 in
        p_out_write.fd <- fd;
        p_out_write.state <- Open
      | `Fd fd ->
        comm p_out_write fd;
      | _ -> pipe p_out_read p_out_write);
      if p_out_read.state = Open then
        Unix.set_close_on_exec p_out_read.fd in

    out p_stdout_write p_stdout_read stdout;
    out p_stderr_write p_stderr_read stderr;

    begin match stdin with
    | `Null ->
      let fd = eintr3 Unix.openfile dev_null [ Unix.O_RDONLY ] 0o400 in
      p_stdin_read.fd <- fd;
      p_stdin_read.state <- Open
    | `Fd fd ->
      comm p_stdin_read fd;
    | _ -> pipe p_stdin_read  p_stdin_write;
    end;

    if p_stdin_write.state = Open then
      Unix.set_close_on_exec p_stdin_write.fd;
  in

  let pid =
    eintr6
      Unix.create_process_env prog args env
      p_stdin_read.fd p_stdout_write.fd p_stderr_write.fd in

  close_pipe p_stdout_write;
  close_pipe p_stderr_write;
  close_pipe p_stdin_read;

  let f_read r =
    let is_stdout =
      if r = p_stderr_read.fd then
        false
      else (
        assert ( r = p_stdout_read.fd );
        true
      )
    in
    let x = try eintr4 Unix.read r tmp_str 0 str_buffer_len with | _ -> -1 in
    if x <= 0 then (
      if is_stdout then
        close_pipe p_stdout_read
      else
        close_pipe p_stderr_read
    )
    else (
      match if is_stdout then stdout else stderr with
      | `Fd _
      | `Null
      | `Stdout
      | `Stderr -> ()
      | `Buffer b -> Buffer.add_substring b (Bytes.unsafe_to_string tmp_str) 0 x
      | `Fun (f: string -> unit) -> f (Bytes.sub_string tmp_str 0 x)
    ) in
  let to_write = match stdin with
  | `Fd _
  | `String ""
  | `Null -> close_pipe p_stdin_write; ref ""
  | `String str -> ref str in
  while p_stdout_read.state = Open ||
        p_stderr_read.state = Open || p_stdin_write.state = Open do
    let wl = if p_stdin_write.state = Open then [p_stdin_write.fd] else [] in
    let rl = if p_stderr_read.state = Open then [p_stderr_read.fd] else [] in
    let rl = if p_stdout_read.state = Open then p_stdout_read.fd :: rl else rl in
    let r,w,_ = eintr4 Unix.select rl wl [] 3. in
    List.iter f_read r ;
    match w with
    | [] -> ()
    | [fd] ->
      assert (p_stdin_write.fd = fd);
      let str_len = String.length !to_write in
      assert (str_len > 0 );
      let n_written = eintr4 Unix.write_substring fd !to_write 0 str_len in
      if n_written >= str_len then (
        to_write := "";
        close_pipe p_stdin_write
      )
      else
        to_write := String.sub !to_write n_written (str_len - n_written)
    | _ -> assert false
  done;
  close_pipe p_stdout_read;
  close_pipe p_stderr_read;

  let _, process_status = eintr2 Unix.waitpid [] pid in
  let ret_code = match process_status with
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED _ -> 2 (* like OCaml's uncaught exceptions *)
  | Unix.WSTOPPED _ ->
    (* only possible if the call was done using WUNTRACED
       or when the child is being traced *)
    3  in
  ret_code

let cmd_to_string prog args =
  let args = List.map Filename.quote args in
  String.concat " " (prog::args)
