---
layout: default
title: Overview
nav_order: 1
---

# Overview

ppx_cstubs is a ppx-based preprocessor for stub generation with
[ctypes](https://github.com/ocamllabs/ocaml-ctypes).

ppx_cstubs creates two files from a single ml file: a file with c stub
code and a ml file with all additional boilerplate code.

## Contents

* [Calling C functions from OCaml](./functions.md)
* [Enum Types](./enums.md)
* [Struct and Union Types](./structures.md)
* [Dealing with Opaque Types and Integers](./opaque.md)
* [Retrieving Constants](./constants.md)
* [C #include Guards](./header.md)
* [Static OCaml Callbacks](./static_callbacks.md)
* [Pointers to C Objects](./foreign_value.md)
* [Scoping Rules](./scope.md)

## not yet documented

Consult the
[README.md](https://github.com/fdopen/ppx_cstubs/blob/master/README.md)
in the meanwhile...

* merlin (how to configure your editor? `ppx_cstubs.merlin` can only
  be used inside your editor, not inside build rules)
* compilation
* dune integration - see also this [example](https://github.com/fdopen/ppx_cstubs/tree/master/examples/dune-project)
* cross package dependencies with `ppx_cstubs -pkg ...`
