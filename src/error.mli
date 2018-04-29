(** Error type and Error handling functions *)
type 'lc t = {
  err_loc:'lc; (** Error localization *)
  err_txt:string (** Error message *)
}
(** The type for errors *)

type 'a t_result = ('a,Utils.loc t) result
(** The type for computation results *)

val print_error : Utils.loc t -> unit
(** [print_error err] prints [err] in standard error output.*)

exception Error of Utils.loc t

val raise_exn : Utils.loc -> string -> 'a
(** [raise_exn err_loc err_msg] raises the exception [Error {err_loc;err_msg}]. *)

val bind_res: ('a,'err) result -> ('a -> ('b,'err) result) -> ('b,'err) result
val fold_left : ('a -> 'b -> ('a,'c) result) -> 'a -> 'b list -> ('a,'c) result
val list_iter : ('a -> (unit,'b) result) -> 'a list -> (unit,'b) result
