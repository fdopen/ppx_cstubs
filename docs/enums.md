---
layout: default
title: Enum Types
nav_order: 3
---

# Enum Types

Enumerations can be written as sum types with only constants:

```c
enum day {Mon, Tue, Wed, Thur, Fri, Sat, Sun};
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
```

In the above case an ordinary OCaml sum type named `day` will be created
as well as a value `day` of type `day Ctypes.typ`.


In some cases, further annotations are required due to the different
syntax rules of OCaml and C:

```c
typedef enum {working = 1, failed = 0} State;
```

```ocaml
type%c state =
| Working [@cname "working"]
| Failed [@cname "failed"]
[@@cname "State"] [@@typedef]
```

If your function returns something else, e.g. `3` instead of `working` or
`failed`, an exception would be thrown. You can suppress this
behaviour by providing a custom "unexpected" function of type `int64
-> your_type`:

```ocaml
type%c state =
| Working
| Failed
[@@unexpected fun x -> Printf.printf "oops, %Ld returned\n%!" x; Failed]
```

## Bit Masks

`[@@as_bitmask]` can be added to the type definition for functions that
expect a bitwise-inclusive OR of enumeration constants (similar to
[open](https://pubs.opengroup.org/onlinepubs/009695399/functions/open.html)):

```c
enum foo {
    F1 = (1u << 0),
    F2 = (1u << 1),
    F3 = (1u << 2)
};
 ```

```ocaml
type%c foo =
 | F1
 | F2
 | F3 [@@as_bitmask] 
 [@@ unexpected_bits fun (matched:foo list) (remainig_bits:int64) -> ... ]
```

In this case `foo` will be of type `foo list Ctypes.typ`.
`[@@with_bitmask]` would create two values: `foo` of type `foo
Ctypes.typ` and `foo_bitmask` of type `foo Ctypes.typ`.
