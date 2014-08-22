#!/usr/bin/env ocaml 
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "tiger2ml" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/tigerLib";
    Pkg.bin ~auto:true "src/tiger2ml";
    Pkg.doc "README.md" ]
