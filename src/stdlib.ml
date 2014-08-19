let _print =
  print_string

let _printi =
  print_int

let _flush () =
  flush stdout

let _getchar () =
  let c = input_char stdin in
    (String.make 1 c)

let _ord s =
  let c = s.[0] in
    int_of_char c

let _chr n =
  let s = char_of_int n in
    String.make 1 s

let _size =
  String.length

let _substring =
  String.sub

let _concat s1 s2 =
  s1 ^ s2

let _not n =
  if n = 0 then 1 else 0

let _exit n =
  ignore (exit n)
