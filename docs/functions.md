---
layout: default
title: Calling C Functions
nav_order: 2
---

# Calling C Functions

ppx\_cstubs abuses `external` declarations that are not used in regular
code. Instead of OCaml types, values of type
[Ctypes.typ](https://github.com/ocamllabs/ocaml-ctypes/blob/066cf8fa0039fa03287fcf784533e7c0e460fc54/src/ctypes/ctypes_types.mli#L17)
are used inside the declarations:

```c
/* function prototypes */
int puts(const char *s);
char *getenv(const char *name);
```

To access these functions from OCaml, you simply repeat the prototypes in
OCaml syntax with appropriate `Ctypes.typ` values:

```ocaml
external puts: string -> int = "puts"
external getenv: string -> string_opt = "getenv"

let () = (* regular code *)
  match getenv "HOME" with
  | None -> ()
  | Some s ->
    let i = puts s in
    ...
```

## Attributes for External Declarations

External declarations can be annotated with three different
attributes:

```ocaml
external foo : void -> bar = "cfoo" [@@ release_runtime_lock] [@@ return_errno] [@@ noalloc]
```

- **release_runtime_lock**: If `[@@ release_runtime_lock]` is
  specified, the OCaml runtime lock will be released during the call
  to the C function, allowing other threads to run. You can't pass
  arguments that point to the OCaml heap (like `Ctypes.ocaml_string`)
  to such functions.

- **return_errno**: If `[@@ return_errno]` is given, the function
  returns a pair as result. The first value is the regular result, the
  second value is the errno code of type `Signed.sint`.

- **noalloc**: If the C function doesn't interact with the OCaml
  runtime, e.g. by calling a function you have passed to C, you can
  add `[@@ noalloc]` to the declaration. The generated code will be
  slightly faster. Note: `noalloc` is here intended as an attribute
  for your C function, not for the generated stub code and the C
  function together. You can add it, even if you (believe to) know
  that the generated stub code has to allocate memory in the OCaml
  heap. The generated code will still differ.
  
## Pseudo-Types

Inside external declarations (and similar locations like [struct
definitions](./structures.md)) the following parameterized
(pseudo)types can be used:

* [ptr](https://github.com/ocamllabs/ocaml-ctypes/blob/b19b190ad5083d03130dd67508705da77c1c5089/src/ctypes/ctypes_types.mli#L177)
* [ptr\_opt](https://github.com/ocamllabs/ocaml-ctypes/blob/b19b190ad5083d03130dd67508705da77c1c5089/src/ctypes/ctypes_types.mli#L181)
* [funptr](https://github.com/ocamllabs/ocaml-ctypes/blob/b19b190ad5083d03130dd67508705da77c1c5089/src/ctypes-foreign-threaded/foreign.mli#L49)
* [funptr\_opt](https://github.com/ocamllabs/ocaml-ctypes/blob/b19b190ad5083d03130dd67508705da77c1c5089/src/ctypes-foreign-threaded/foreign.mli#L80)
* [static\_funptr](https://github.com/ocamllabs/ocaml-ctypes/blob/b19b190ad5083d03130dd67508705da77c1c5089/src/ctypes/ctypes_types.mli#L374)

So instead of ...
```ocaml
(* function prototype in c:
   void *bsearch(const void *key, const void *base,
                 size_t nmemb, size_t size,
                 int (*compar)(const void *, const void *));
*)
let%c compar = funptr (ptr void @-> ptr void @-> returning int)
let%c ptr_void = ptr void;
external bsearch:
  key: ptr_void
  -> base: ptr_void
  -> nmemb: size_t
  -> size: size_t
  -> compar
  -> ptr_void = "bsearch"
```
... one can also write:
```ocaml
external bsearch:
  key:void ptr
  -> base: void ptr
  -> nmemb: size_t
  -> size: size_t
  -> (void ptr -> void ptr -> int) funptr
  -> void ptr = "bsearch"
```

## Inline Code

As a slight extension of the scheme above, you can also label your
parameters, annotate external (`%c`) and write a few lines of C code:

```ocaml
external%c puts_flush : str:string -> bool = {|
  int r = puts($str);  /* to escape a regular dollar sign: $$ */
  if ( r < 0 ){
    return false;
  }
  r = fflush(stdout);
  return (r == 0); /* `return` is mandatory, unless your function is void */
|} [@@ release_runtime_lock]

let _ : int = puts_flush ~str:"Hello World"
```

This way several switches between the OCaml runtime and C are avoided,
which has various advantages:

- Intermediate results can be stored on the C stack. They don't need
  to be allocated on the heap and wrapped in a way to appease the
  OCaml runtime.

- The C compiler can better optimise your code.

- Constant parameters don't need to be exposed to OCaml, just to pass
  them to the C function.

- You often have to write (and generate) less code, if you don't
  create wrappers for every c function and type, but just wrap
  snippets of C code.

### Implicit Removal of Labels

Labels that end with an underscore will be removed from the generated
OCaml function:

```ocaml
external%c puts_flush : str_:string -> bool = {|
  int r = puts($str_);
  ...
|}

let _ : int = puts_flush "Hello World" (* no warning about a missing label ever *)
```

Labels are always removed, when an operator is defined through inline code.
