OCAMLBUILD = ocamlbuild
OCAMLBUILDFLAGS = -classic-display -use-ocamlfind -use-menhir

native:
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) src/tc.native

byte:
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) src/tc.byte

clean:
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) -clean

.PHONY: native byte clean
