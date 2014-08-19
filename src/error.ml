open Syntax

exception Error of Lexing.position * string

let error p s =
  raise (Error (p, s))

(*let get_file_line p =
  let ic = open_in p.pos_filename in begin
    seek_in ic (p.pos_offset-p.pos_column);
    let line = input_line ic in begin
      close_in ic;
      line
    end
  end*)
