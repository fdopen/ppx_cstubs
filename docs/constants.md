---
layout: default
title: Constants
nav_order: 6
---

# Constants

Compile-time constants can be retrieved from C code and inserted it
into your OCaml code at an arbitrary location:

```c
#define FOOBAR 30
```

```ocaml
let _FOOBAR = [%c constant "FOOBAR" camlint]
let () =
  let _FOOBAR_X3 = 3 * [%c constant "FOOBAR" camlint] in
  ...
```

Note: Const-qualified objects (of any type) are not constants. It only
works across all compilers for enums and constant expressions that are
usually exposed through macros.

Only integers and string literals can be retrieved. Integer values are
checked for over- and underflows that trigger failures at compile
time. Try to compile your code on a 32-bit system and under a
different platform before you release it 😉. If you don't care about
overflows, you can just cast the value to the appropriate type: `[%c
constant "(int)BAR" int]`.

Values of any other type can be imported at runtime with [`[%c foreign_value ...]`](./foreign_value.md).

## Let-bound Constants

Sometimes it is necessary to use constants already for defining ctypes expression, e.g.

```c
struct ms {
    int x;
    char y[YLENGTH];
};
```

Therefore, another syntax is also supported:

```ocaml
let%c _YLENGTH = constant "YLENGTH" camlint
let%c char_y_ar = array _YLENGTH char
type%c ms = {
  x : int;
  y : char_y_ar;
}
```

There are however several disadvantages associated with this syntax:
* it only works for integers and not for string literals
* When you are cross-compiling, you can only extract integers that are
  representable in the build and target platform. Normally only the
  target platform matters
* code generation is slower and the error messages are less accurate
