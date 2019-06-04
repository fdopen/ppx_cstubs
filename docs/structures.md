---
layout: default
title: Structs and Unions
nav_order: 4
---

# Structs

There is a special syntax for creating and accessing C structs:

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
  x: int; (* int here refers to the value Ctypes.int, not the type int *)
  y: int;
}
```

The above code will be translated to something along the following lines:
```ocaml
type point
let point : point Ctypes.structure Ctypes.typ = Ctypes.structure "point"
let x = Ctypes.field point "x" Ctypes.int
let y = Ctypes.field point "y" Ctypes.int
let () = Ctypes.seal point
 ```

Example usage:
```ocaml
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

It's also possible to convert OCaml records to C structs on the fly by
annotating the type declaration with `[@@ as_record]`:

```ocaml
type%c point = {
  x: int;
  y: int;
} [@@ as_record]
(* [@@ with_record] will create two values: point and point_record *)

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

`[@@ with_record]` is useful, when easier access to the
fields is only temporary needed for debugging.
[Ctypes.coerce](https://github.com/ocamllabs/ocaml-ctypes/blob/b19b190ad5083d03130dd67508705da77c1c5089/src/ctypes/ctypes.mli#L440)
can be used to convert between the different pointer representations:

```ocaml
let {x,y} = !@(coerce (ptr point) (ptr point_record) t)
```

## Unions

Unions can declared in a similar way:

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

## Nested Structs and Unions

Structs that contain other structs or unions are best accessed by
creating a "flat" OCaml record:

```c
struct s {
  int tag;
  union {
    int i;
    unsigned int u;
  } d;
};
```

```ocaml
type%c s = { 
  tag : int;
  d_i : sint [@cname "d.i"];
  d_u : uint [@cname "d.u"];
}
 ```
