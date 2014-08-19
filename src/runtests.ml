(* Copyright (c) 2014, Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
   REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
   AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
   INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
   LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
   OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
   PERFORMANCE OF THIS SOFTWARE. *)

let is_tiger_file s =
  let len = String.length s in
  if len < 4 then false
  else (String.sub s (len-4) 4) = ".tig"

let is_error_test s =
  let len = String.length s in
  if len < 8 then false
  else (String.sub s 0 8) = "/* error"

let get_base () =
  if Array.length Sys.argv = 1 then "."
  else Sys.argv.(1)

let _ =
  let base = get_base () in
  let dir = Unix.opendir base in
  let total = ref 0 in
  let passed = ref 0 in
  try while true do
    let rec loop next =
      if is_tiger_file next then begin
        let next_ic = open_in (base ^ next) in
        let line = input_line next_ic in
        let should_fail = is_error_test line in
        Printf.printf "%s:\n" next;
        let did_fail = (Sys.command ("./ti.native " ^ (base ^ next)) != 0) in
        let pass = (should_fail && did_fail) || (not should_fail && not did_fail) in
          close_in next_ic;
          incr total;
          Printf.printf "%s:\t%s\n" next (if pass then "PASS" else "FAIL");
          if pass then incr passed
      end else loop (Unix.readdir dir)
    in loop (Unix.readdir dir)
  done with End_of_file -> Unix.closedir dir;
  Printf.printf "Testing finished. %d/%d passed.\n" !passed !total
