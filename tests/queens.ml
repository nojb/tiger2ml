type intArray = int array

let main () =
let n = 8 in
let row = Array.make n 0 in
let col = Array.make n 0 in
let diag1 = Array.make (n + n - 1) 0 in
let diag2 = Array.make (n + n - 1) 0 in
let printboard () =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      print_string (if col.(i) = j then " O" else " .")
    done;
    print_string "\n"
  done;
  print_string "\n"
in let rec tri c =
  if c = n then printboard ()
  else for r = 0 to n - 1 do
    if row.(r) = 0 && diag1.(r+c) = 0 && diag2.(r+(n-1)-c) = 0 then begin
      row.(r) <- 1;
      diag1.(r+c) <- 1;
      diag2.(r + (n - 1) - c) <- 1;
      col.(c) <- r;
      tri (c+1);
      row.(r) <- 0;
      diag1.(r+c) <- 0;
      diag2.(r + (n - 1) - c) <- 0
    end
  done
in tri 0

let _ = main ()
