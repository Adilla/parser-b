
let warn lc msg =
  let open Lexing in
  Printf.fprintf stderr "[file: %s;line: %i;column: %i] %s\n"
    lc.pos_fname lc.pos_lnum (lc.pos_cnum-lc.pos_bol+1) msg

exception Fatal

let error lc msg =
  let open Lexing in
  Printf.fprintf stderr "[file: %s;line: %i;column: %i] %s\n"
    lc.pos_fname lc.pos_lnum (lc.pos_cnum-lc.pos_bol+1) msg;
  raise Fatal
