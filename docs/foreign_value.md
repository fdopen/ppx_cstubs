---
layout: default
title: Pointers to C Objects
nav_order: 8
---

# Pointers to C Objects

Pointers to C objects can be retrieved with a function similar to
[Foreign.foreign_value](https://github.com/ocamllabs/ocaml-ctypes/blob/e192f74421c2755c51ba90dfac19b9593fa72df9/src/ctypes-foreign-unthreaded/foreign.mli#L44):

```c
extern char **environ; /* see `man 7 environ` */
```

```ocaml
let () =
  let environ = [%c foreign_value "environ" (ptr string_opt)] in
  let rec iter env =
    match !@env with
    | None -> ();
    | Some s ->
      print_endline s;
      iter (env +@ 1)
  in
  iter !@environ
```

This feature can also be used to import const qualified objects or
double literals at runtime:

```ocaml
let%c () = header {|
#include <math.h>
static const double m_2_sqrtpi = M_2_SQRTPI;
|}

let _M_2_SQRTPI = !@ [%c foreign_value "m_2_sqrtpi" double]
```
