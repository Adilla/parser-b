type 'lc t = { err_loc:'lc; err_txt:string }
type 'a t_result = ('a,Utils.loc t) result

val print_error : Utils.loc t -> unit

exception Error of Utils.loc t
val raise_exn : Utils.loc -> string -> 'a

val bind_res: ('a,'err) result -> ('a -> ('b,'err) result) -> ('b,'err) result
val fold_left : ('a -> 'b -> ('a,'c) result) -> 'a -> 'b list -> ('a,'c) result
