---
layout: default
title: Opaque Types
nav_order: 5
---

# Opaque Types

ppx_cstubs provides various helper to deal with values whose types
differ from platform to platform or that are kept opaque.

## Abstract

```ocaml
let%c myabstract = abstract "foo"
```

The statement above will be translated to something along the
following lines - with `size` and `alignment` determined
automatically:

```ocaml
type myabstract
let myabstract : myabstract Ctypes.abstract Ctypes.typ = 
   Ctypes.abstract ~size ~alignment ~name:"foo"
```


`abstract` is most suitable for structs and unions, whose members are
private or don't need to be accessed from OCaml. `abstract` guarantees
an uniform representation of the values inside the OCaml heap and
functions like
[Ctypes.addr](https://github.com/ocamllabs/ocaml-ctypes/blob/b19b190ad5083d03130dd67508705da77c1c5089/src/ctypes/ctypes.mli#L435)
can be applied to its values.

References: 
* [ctypes.mli](https://github.com/ocamllabs/ocaml-ctypes/blob/b19b190ad5083d03130dd67508705da77c1c5089/src/ctypes/ctypes.mli#L101)
* [ctypes\_types.mli](https://github.com/ocamllabs/ocaml-ctypes/blob/b19b190ad5083d03130dd67508705da77c1c5089/src/ctypes/ctypes_types.mli#L330)

## Opaque

```ocaml
let%c myopaque = opaque "foo"
(* 'myopaque' will be of type 'myopaque Ctypes.typ' *)
```

`opaque` is similar to `abstract`, but will choose the best memory
representation available for the type. If `foo` turns out to be an
integer type, it will be an integer inside OCaml. If it is a pointer,
it will be of type `Ctypes.ptr` - or `Ctypes.abstract`
otherwise. `opaque` is preferable to `abstract`, if values of the
corresponding type are usually passed by value.


## Integers of Unknown Size and Signedness

```ocaml
module Signed_t = [%c int "foo"]
module Unsigned_t = [%c uint "foo"]
module Unknown_signed = [%c aint "foo"]
external foo : Signed_t.t -> void
let f t = Signed.add (Signed_t.of_int (-42)) t
```

`module Signed_t = [%c int "foo"]` creates an module `Signed_t` with
the signature of
[Signed.S](https://github.com/ocamllabs/ocaml-integers/blob/41846f424b13af552200939228492d29bd06a495/src/signed.mli#L10),
but also include an additional value `t` of type `t
Ctypes.typ`. `Signed_t.t` can be used inside external declarations and
similar locations, the other values of the module are however abstract
during code generation.

`[%c uint "foo"]` will create an analogous
module with a signature of
[Unsigned.S](https://github.com/ocamllabs/ocaml-integers/blob/41846f424b13af552200939228492d29bd06a495/src/unsigned.mli#L10).

`[%c aint "foo"]` will also be of type `Unsigned.S`, but with an
additional value `min_int` that can be used to easily determine, if the
type is signed or not. `[%c int "foo"]` and `[%c uint "foo"]` will
throw an error during preprocessing, if the underlying type differs in
signedness.

Integer types larger than 8 bytes are not supported.
