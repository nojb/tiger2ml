OCAMLBUILD = ocamlbuild
OCAMLBUILDFLAGS = -classic-display -use-ocamlfind

native:
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) src/tigerMain.native

byte:
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) src/tigerMain.byte

clean:
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) -clean

.PHONY: native byte clean
