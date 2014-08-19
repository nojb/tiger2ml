let rec nfib = function
  | 0 -> 1
  | 1 -> 1
  | n -> (nfib (n - 1)) + (nfib (n - 2)) + 1

let _ =
  print_endline (string_of_int (nfib 15))
