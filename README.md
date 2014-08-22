tiger2ml - a Tiger-to-OCaml translator
----------------------------------------

`tiger2ml` is a source-level translator from the [Tiger][] programming language
to [OCaml][].  The resulting files can be compiled with the OCaml compilers to
obtain byte- or native-code executables.  It directly generates an OCaml AST by
using the `compiler-libs` library distributed with the OCaml compilers.

It is distributed under the Q Public License, version 1.0 (same as the OCaml
compilers).

Contact: [Nicolas Ojeda Bar][]

[Nicolas Ojeda Bar]: n.oje.bar@gmail.com
[Tiger]: http://www.cs.princeton.edu/~appel/modern/ml/
[OCaml]: http://ocaml.org

## Example

See below for installation instructions.  Here we suppose that the preprocessor
`tiger2ml` and its accompanying Findlib library `tigerLib` has been successfully
installed.  Suppose that `sum.tig` contains the following code.

    let function sum (n : int) : int =
      let
        var s := 0 var i := 0
      in
        while 1 do
          if i >= n then break
          else (s := s + i; i := i + 1);
        s
      end
    in
      printi (sum (100));
      print ("\n")
    end

We can run the preprocessor on this file

    tiger2ml sum.tig > sum.ml

The file `sum.ml` will contain the following.

    let main () =
      let rec sum_0 n_1 =
        let s_2 = ref 0 in
        let i_3 = ref 0 in
        (try
           while 1 <> 0 do
             if (!i_3) >= n_1
             then raise TigerLib.Break
             else (s_2 := ((!s_2) + (!i_3)); i_3 := ((!i_3) + 1)) done
         with | TigerLib.Break  -> ());
        !s_2 in
      TigerLib.printi (sum_0 100); TigerLib.print "\n"
    let _ = TigerLib.run main

This file can be compiled to native code by running

    ocamlfind opt -package tigerLib sum.ml -o sum
    ./sum
    4950

Alternatively, we can translate the file into OCaml and compile it all in one go
by running

    ocamlfind opt -package tigerLib -pp tiger2ml -impl sum.tig -o sum

## Installation

`tiger2ml` can be installed using [OPAM][].

    opam install tiger2ml

This will install the actual preprocessor, `tiger2ml`, as well as a OCaml
library with Findlib name `tigerLib` which is needed to compile the generated
OCaml programs.

[OPAM]: https://opam.ocaml.org

