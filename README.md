# ppx_cstubs

a ppx-based preprocessor for quick and dirty stub generation with
[ctypes](https://github.com/ocamllabs/ocaml-ctypes).

ppx_cstubs creates two files from a single ml file: a file with c stub
code and a ml file with all additional boilerplate code.

The preprocessor abuses `external` declarations that are not used in
regular code. Instead of OCaml types, values of type
[Ctypes.typ](https://github.com/ocamllabs/ocaml-ctypes/blob/066cf8fa0039fa03287fcf784533e7c0e460fc54/src/ctypes/ctypes_types.mli#L17)
are used inside the declarations:

```c
/* function prototypes */
int puts(const char *s);
char *getenv(const char *name);
```

To access these functions from OCaml, you simply repeat the prototype in
OCaml syntax and with appropriate `Ctypes.typ` values:

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

To generate your stub and ml file:

```bash
ppx_cstubs myfile.c.ml -o-c myfile_stubs.c -o-ml myfile.ml
```


## inline code

As a slight extension of the scheme above, you can also label your
parameters, annotate external (`%c`) and write a few lines of C code:

```ocaml
external%c puts_flush : str:string -> bool = {|
  int r = puts($str);
  if ( r < 0 ){
    return false;
  }
  r = fflush(stdout);
  return (r == 0); /* `return` is mandatory, unless your function is void */
|} [@@ release_runtime_lock]

let _ : bool = puts_flush ~str:"Hello World"
```

This way several switches between the OCaml runtime and C are
avoided. Intermediate results can be stored on the C stack instead of
the heap; constant parameters don't need to allocated inside OCaml,
just to pass them to the C function, and so forth.


## scope and custom types

The types inside external declarations have their own environment.
Normal let-bindings or statements like `open` won't have any affect on
them. (Ppx rewriters don't have access to types and similar
information).

By default only types referenced in
[Ctypes_types.TYPE](http://ocamllabs.io/ocaml-ctypes/Ctypes_types.TYPE.html)
are accessible. It's however possible to create new types that are
then available inside your regular program and inside external
declarations:

```ocaml
let%c int_as_bool = (* all bindings must be of type Ctypes.typ *)
  view
   int (* no access to your regular scope inside the expression *)
   ~read:(fun x -> if x = 0 then false else true)
   ~write:(fun x -> if x = false then 0 else 1)

(* int_as_bool is available in the regular scope *)
let ibptr = Ctypes.allocate int_as_bool true

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

(* alternative syntax: *)
external bsearch:
  key:void ptr
  -> base: void ptr
  -> nmemb: size_t
  -> size: size_t
  -> (void ptr -> void ptr -> int) funptr
  -> void ptr = "bsearch"
(* other pseudo types: ptr_opt, funptr_opt, static_funptr *)
```

## compiling

The generated code must be linked against the findlib package `ppx_cstubs`.


## merlin

`ppx_cstubs.merlin` can be used to inform merlin about the special
syntax. It produces a correctly typed syntax tree faster than the real
preprocessor. (The generated code is however semantically incorrect
and would quit the program with an exception at runtime.)

## details

### enums

Enumerations can be written as sum types with only constants - with
special annotations, if it's required by the OCaml syntax:

```c
enum day {Mon, Tue, Wed, Thur, Fri, Sat, Sun};
typedef enum {working = 1, failed = 0} State;
```

```ocaml
type%c day =
| Mon
| Tue
| Wed
| Thur
| Fri
| Sat
| Sun

type%c state =
| Working [@cname "working"]
| Failed [@cname "failed"]
[@@ cname "State"] [@@ typedef]
```

### structs

There is also a special syntax for creating and accessing c structs:

```c
struct point {
    int x;
    int y;
};

/* example functions */
struct point add (struct point a, struct point b) {
  struct point res;
  res.x = a.x + b.x ;
  res.y = a.y + b.y ;
  return res;
}

void add_ptr(struct point *a, struct point *b, struct point *res){
  res->x = a->x + b->x;
  res->y = a->y + b->y;
}
```

```ocaml

type%c point = {
  x: int;
  y: int;
}

(* syntax for something along the following lines:
   let point = Ctypes.structure "point"
   let x = Ctypes.field point "x" Ctypes.int
   let y = Ctypes.field point "y" Ctypes.int
   let () = Ctypes.seal point *)

let () =
  let p1 = Ctypes.make point in
  Ctypes.setf p1 x 1;
  Ctypes.setf p1 y 2;
  let p2 = Ctypes.make point in
  Ctypes.setf p2 x 3;
  Ctypes.setf p2 y 4;
  let p3 = add p1 p2 in
  Printf.printf "add (simple): %d;%d\n" (getf p3 x) (getf p3 y);
  let res = Ctypes.make point in
  let () = add_ptr (Ctypes.addr p1) (Ctypes.addr p2) (Ctypes.addr res) in
  Printf.printf
    "add_ptr (simple): %d;%d\n"
    (Ctypes.getf res x)
    (Ctypes.getf res y)
```

It's also possible to convert OCaml records to c structs on the fly by
annotating the type declaration with `[@@ as_record]`:

```ocaml

type%c point = {
  x: int;
  y: int;
} [@@ as_record]
(* [@@ with_record] will create two types: point and point_record *)

external add: point -> point -> point = "add"
external add_ptr: point ptr -> point ptr -> point ptr -> void = "add_ptr"

let () =
  let p1 = { x = 1 ; y = 3} in
  let p2 = { x = 2 ; y = 4} in
  let p3 = add p1 p2 in
  Printf.printf "add (record): %d;%d\n" p3.x p3.y;
  let p1_ptr = allocate point p1 in
  let p2_ptr = allocate point p2 in
  let res_ptr = allocate_n point ~count:1 in
  let () = add_ptr p1_ptr p2_ptr res_ptr in
  let res = !@ res_ptr in
  Printf.printf "add_ptr (record) %d;%d\n" res.x res.y
```

If the struct contains pointers, you have to be careful that the
garbage collector doesn't free the memory behind your back. The
generated code for automatic boxing and unboxing can't handle
such subtle issues for you.


### unions

unions can declared in a similar way:

```c
typedef union {
    long l;
    double d;
} data;
```

```ocaml
type%c_union data = {
  l: long;
  d: double;
} [@@ typedef]
```


### constants

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
works for enums and constant expressions that are usually exposed through
macros.

Only integer and string literals can be retrieved. Integer values are
checked for overflows that will trigger an error at compile time. Try
to compile your code on a 32-bit system and under a different platform
before you release it 😉

Values of any other kinds can be imported at runtime with `[%c
foreign_value ... ]`, as explained below.


### c headers

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

### pointers to C objects

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

### attributes for external declarations

External declarations can be annotated with three different
attributes:

```ocaml
external foo : void -> bar = "cfoo" [@@ release_runtime_lock] [@@ return_errno] [@@ noalloc]
```

- **release_runtime_lock**: If `[@@ release_runtime_lock]` is
  specified, the OCaml runtime lock should be released during the call
  to the C function, allowing other threads to run. You can't pass
  arguments, that point to the OCaml heap (like `Ctypes.ocaml_string`)
  to such functions.
- **return_errno**: If `[@@ return_errno]` is given, the function
  returns a pair as result. The first value is the regular result, the
  second value is the errno code of type `Signed.sint`.
- **noalloc**: If the C function doesn't interact with the OCaml
  runtime, e.g. by calling a callback you have provided, you can add
  `[@@ noalloc]` to the declaration. The generated code will be
  slightly faster. Note: `noalloc` is here intended as an attribute
  for your c function, not for the generated stub code and the c
  function. You can add it, even if you (believe to) know, that the
  generated stub code must allocated memory in the OCaml heap. The
  generated code will still differ.
