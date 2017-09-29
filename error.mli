exception Error of Utils.loc * string
val print_error : Utils.loc -> string -> unit

type 'a t_error = ('a,Utils.loc*string) result

val bind: 'a t_error -> ('a -> 'b t_error) -> 'b t_error
val (>>=): 'a t_error -> ('a -> 'b t_error) -> 'b t_error

val fold_left : ('a -> 'b -> ('a,'c) result) -> 'a -> 'b list -> ('a,'c) result
