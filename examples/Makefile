.PHONY: clean default

OCAML_CONFIG:=$(shell ocamlfind c -config | tr -d '\015')
OCAML_VERSION_MAJOR=$(word 1, $(subst ., ,$(OCAML_VERSION)))
OCAML_VERSION_MINOR=$(word 2, $(subst ., ,$(OCAML_VERSION)))
OCAML_VERSION:=$(shell ocamlfind c -config | awk '/^version/ {print $$2}')
OCAML_SYSTEM:=$(shell ocamlfind c -config | awk '/^system/ {print $$2}')
ifeq ($(OCAML_VERSION_MAJOR).$(OCAML_VERSION_MINOR),4.02)
EXAMPLES=
else
ifeq ($(OCAML_SYSTEM),linux)
EXAMPLES=inline.exe time.exe getpwent.exe
else
EXAMPLES=inline.exe time.exe
endif
endif

default: ftw.exe $(EXAMPLES)


.PHONY: help
help:
	echo $(OCAML_VERSION_MAJOR)
	echo $(OCAML_VERSION_MINOR)
	echo $(OCAML_SYSTEM)

%.ml %_stubs.c: %.c.ml
	@dune exec --no-print-directory --display=quiet -- ppx_cstubs $< -pretty -o-ml $(basename $@).ml -o-c $(basename $@)_stubs.c

%.exe: %_stubs.c %.ml
	@dune exec --no-print-directory --display=quiet -- ocamlfind opt -package ctypes,ctypes.foreign,ppx_cstubs -linkpkg $^ -o $@

clean:
	rm -f *.obj *.o *.cm* *.exe *_stubs.c ftw.ml inline.ml time.ml getpwent.ml
