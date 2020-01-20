---
layout: default
title: Scope
nav_order: 9
---

# Scope

The types inside external declarations have their own environment.
Normal let-bindings or statements like `open` won't have any effect on
them. (Ppx rewriters don't have access to types and similar
information).

By default only `Ctypes.typ` values of
[Ctypes_types.TYPE](http://ocamllabs.io/ocaml-ctypes/Ctypes_types.TYPE.html)
are accessible (the corresponding module is opened by default). It's
however possible to create new types that are then available inside
your regular program and inside external declarations:

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
let compar = 3 (* has no effect on the following external declaration *)
external bsearch:
  key: ptr_void
  -> base: ptr_void
  -> nmemb: size_t
  -> size: size_t
  -> compar
  -> ptr_void = "bsearch"
```

This also means that you can't reference any `Ctypes.typ`s that you've
created in other files of your current project.

## Cross Package Dependencies

Types from other libraries can however be made accessible: `ppx_cstubs
-pkg foo ...` will make the types of the findlib library `foo`
available to the preprocessor (it also works for plain `.cma` or
`.cmo` files). You just have to ensure that your regular build
instructions and the flags that are passed do `ppx_cstubs` are
consistent.
