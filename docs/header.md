---
layout: default
title: Headers
nav_order: 7
---

# C Headers

The necessary C headers files can be included through the `header`
pseudo-function. The code will be removed from the generated ml file
and only appear in the c file:

```ocaml
let%c () = header {|
#include <stdio.h>
|}
external puts: string -> int = "puts"

(* You can also add arbitrary c code there: *)
let%c () = header {|
#define put_own(s) \
  ...
|}

external puts_own: string -> int = "puts_own"
```

The generated c file is of course flat and without any scope. It might be
good idea to only add a single `let%c () header {| ... |}` statement
at the top of your file and don't spread several statements across
your file or in different modules. Otherwise a casual reader of your
code might draw a wrong conclusion ...
