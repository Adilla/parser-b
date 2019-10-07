type 'lc t = { err_loc:'lc; err_txt:string }
type 'a t_result = ('a,Utils.loc t) result

exception Error of Utils.loc t

let raise_exn err_loc err_txt = raise (Error { err_loc; err_txt })

let print_error err=
  let open Lexing in
  Printf.fprintf stderr "[file: %s;line: %i;column: %i] %s\n"
    err.err_loc.pos_fname err.err_loc.pos_lnum
    (err.err_loc.pos_cnum-err.err_loc.pos_bol+1) err.err_txt

let warn lc msg =
  let open Lexing in
  Printf.fprintf stderr "[file: %s;line: %i;column: %i] %s\n"
    lc.pos_fname lc.pos_lnum (lc.pos_cnum-lc.pos_bol+1) msg

let bind_res res f =
  match res with
  | Ok x -> f x
  | Error _ as err -> err

let rec fold_left (f:'a -> 'b -> ('a,'c) result) (acc:'a) (lst:'b list) : ('a,'c) result =
  match lst with
  | [] -> Ok acc
  | hd::tl ->
    begin match f acc hd with
      | Ok acc -> fold_left f acc tl
      | Error _ as err -> err
    end

let rec list_iter f = function
  | [] -> Ok ()
  | hd::tl ->
    begin match f hd with
      | Ok _ -> list_iter f tl
      | Error _ as e -> e
    end
