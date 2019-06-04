---
layout: default
title: Static Callbacks
nav_order: 7
---

# Static OCaml callbacks

The usual way of passing callbacks to C is through
[Foreign.funptr](https://github.com/ocamllabs/ocaml-ctypes/blob/b19b190ad5083d03130dd67508705da77c1c5089/src/ctypes-foreign-threaded/foreign.mli#L49).

`Foreign` internally relies on `libffi`, that dynamically generates
code at runtime, and `Foreign` will hide all the ugly details from the
OCaml coder. While it is very convenient, it also has its drawbacks:

- It is slow.

- libffi won't work, if security measures like PaX's MPROTECT are
  active - or becomes even slower, when it tries to work around such
  limitations.

Static callbacks are an alternative way to pass OCaml functions to
C. They are particular useful, if the callbacks are short and get
called in a high frequency. Then the overhead introduced by libffi and
`ctypes.foreign's` generic wrapper around it might be too costly. If
the callbacks do more expensive computations or only get called a few
times, `ctypes.foreign` is usually the better choice because of its
easier interface.

With static callbacks it is not possible to pass closures to C
directly. You have to store the context manually and restore it again
inside the callback.

___

The following example code will demonstrate the usage and syntax of
static callbacks through a generic binding to glibc's `qsort_r`:

```c
void qsort_r(void *base, size_t nmemb, size_t size,
             int (*compar)(const void *, const void *, void *),
             void *arg);
```

The `qsort_r()` function sorts an array with `nmemb` elements of size
`size`. The `base` argument points to the start of the array. The
contents of the array are sorted according to a comparison function
pointed to by `compar`. The parameter `arg` can be used to manually
pass user data (e.g. a closure) to the callback (the third parameter
of the comparison function).

We first define a module for the type of the callback function:

```ocaml
module Compar = [%cb ptr void @-> ptr void @-> ptr void @-> returning int]
```

The only public value of the generated module is `Compar.t`, which is
of type [\_abstract Compar.t Ctypes.static_funptr
Ctypes.typ](https://github.com/ocamllabs/ocaml-ctypes/blob/master/src/ctypes/ctypes_types.mli#L371)

`Compar.t` can be used to create values of type `_abstract
Compar.t Ctypes.static_funptr`:

```ocaml
let%cb qsort_callback p1 p2 arg : Compar.t =
  let f = get_closure arg in (* get_closure described later *)
  f p1 p2
```

At this time, everything has normal scope - except the
pseudo-type-constrain. The only restriction is that you must define it
at the "top-level", i.e. not inside functors or local modules. The
preprocessor tries to detect unsafe contexts and will abort code
generation in such cases. If you hit a loophole, an exception will be
thrown at runtime.

We will also use `Compar.t` as regular `Ctypes.typ` inside external
declarations:

```ocaml
external qsort :
     base:void ptr
  -> nmemb:size_t
  -> size:size_t
  -> Compar.t
  -> arg:void ptr
  -> void
  = "qsort_r"

let qsort ~cmp ar =
  let nmemb = CArray.length ar |> Unsigned.Size_t.of_int in
  let size =  CArray.element_type ar |> sizeof |> Unsigned.Size_t.of_int in
  let base = CArray.start ar |> to_voidp in
  let arg = store_closure cmp in
  Fun.protect ~finally:(fun () -> remove_closure arg) (fun () ->
    qsort ~base ~nmemb ~size qsort_callback ~arg)
```

To implement `get_closure`, `store_closure`, and `remove_closure`, you
can use
[Ctypes.ptr\_of\_raw\_address](https://github.com/ocamllabs/ocaml-ctypes/blob/b19b190ad5083d03130dd67508705da77c1c5089/src/ctypes/ctypes.mli#L213),
e.g:

```ocaml
let htl_closure = Hashtbl.create 16

let store_closure =
  let cnt = ref 0 in
  let rec iter n =
    let next = succ n in
    if Hashtbl.mem htl_closure n then
      iter next
    else
      let () = cnt := next in
      n
  in
  fun f ->
    let n = iter !cnt in
    Hashtbl.replace htl_closure n f;
    Nativeint.of_int n |> ptr_of_raw_address

let get_closure ptr =
  raw_address_of_ptr ptr |> Nativeint.to_int |> Hashtbl.find htl_closure

let remove_closure ptr =
  raw_address_of_ptr ptr |> Nativeint.to_int |> Hashtbl.remove htl_closure
```

## Caveat

In less trivial use cases additional precautions are necessary:

- You can usually not throw exceptions inside callbacks. The C code
  does not expect such a jump, its internal state would become
  invalid. If multiple threads are used, it's also possible that you
  are not able to add a handler that could catch your exception in the
  first place. You have to capture all exceptions, save them for later
  and return an appropriate default value.

- The lifetime management of your closures is often complicated. If
  you forget to remove them from your hash table or similar data
  structure, you will leak memory. If you remove them too early, it's
  even more fatal...

## Annotations for Callbacks

Static callbacks can be annotated further:

```ocaml
let%cb your_callback : Callback.t = foo [@@ acquire_runtime_lock]
```

`[@@ acquire_runtime_lock]` must be used, if your callback is called
from a context, where OCaml's runtime lock was released, e.g. via the
`[@@ release_runtime_lock]` annotation of `external` declarations.

```ocaml
let%cb your_callback x : Callback.t =
  let res = foo x in
  res [@@ thread_registration]
```

`[@@ thread_registration]` must be used, if the C library creates new
threads and might execute your callbacks inside those threads. In
order to use `[@@ thread_registration]`, you have to link
`ctypes.foreign` to your program, even if you don't use `Foreign`
otherwise.
