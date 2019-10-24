module L = StackLexer.Make(MacroLexer)

type state = { s:L.state; mutable last:Lexing_Utils.t_token }

let mk_state_from_channel (filename:string) (input:in_channel) : state =
  let macros = MacroTable.make filename (Lexing.from_channel input) in
  seek_in input 0;
  { s=L.mk_state (MacroLexer.mk_state ~filename (Lexing.from_channel input) macros);
    last = Grammar.EOF, Lexing.dummy_pos, Lexing.dummy_pos }

let mk_state_from_string (input:string) : state =
  let macros = MacroTable.make "" (Lexing.from_string input) in
  { s = L.mk_state (MacroLexer.mk_state ~filename:"" (Lexing.from_string input) macros);
    last = Grammar.EOF, Lexing.dummy_pos, Lexing.dummy_pos }

let is_comp_start_exn (s:state) : bool =
  let rec loop accu =
      let next = L.get_token_exn s.s in
      let open Grammar in
      match next with
      | IDENT _, _, _ | COMMA, _, _ | LPAR, _, _ | RPAR, _, _ -> loop (next::accu)
      | BAR, _, _ -> (List.iter (fun x -> L.push_token s.s x) (next::accu); true)
      |  _ -> (List.iter (fun x -> L.push_token s.s x) (next::accu); false)
    in
    loop []

let get_token_exn (s:state) : Lexing_Utils.t_token =
  let tk = match L.get_token_exn s.s with
    | Grammar.LBRA, st, ed as tk ->
      if is_comp_start_exn s then ( Grammar.LBRA_COMP, st, ed )
      else tk
    | tk -> tk
  in
  s.last <- tk;
  tk

let get_last_token (s:state) : Lexing_Utils.t_token = s.last
