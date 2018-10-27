open Lexing_Utils

module Make (Lexer: sig
    type state
    val get_token_exn: state -> t_token
  end) =
struct

  type state = {
    lex_state:Lexer.state;
    mutable stack: t_token list
  }

  let mk_state (s:Lexer.state) : state =
    { lex_state=s; stack=[] }

  let get_token_exn (s:state) : t_token =
    match s.stack with
    | [] -> Lexer.get_token_exn s.lex_state
    | hd::tl -> ( s.stack <- tl; hd )

  let push_token (s:state) (tk:t_token) : unit =
    s.stack <- tk::s.stack

  let preview_token_exn (s:state) : t_token =
    match s.stack with
    | [] ->
      let tk = Lexer.get_token_exn s.lex_state in
      ( push_token s tk; tk )
    | hd::_ -> hd 
end
