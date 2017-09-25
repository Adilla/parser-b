exception Error of Utils.loc * string

let print_error (loc:Utils.loc) (msg:string) : unit =
  let open Lexing in
  Printf.fprintf stdout "[file:%s;line:%i;column:%i] %s\n"
    loc.pos_fname loc.pos_lnum (loc.pos_cnum-loc.pos_bol+1) msg

type 'a t_error = ('a,Utils.loc*string) result

let bind (res:'a t_error) (f:'a -> 'b t_error) =
  match res with
  | Ok x -> f x
  | Error _ as err -> err

let (>>=) = bind

