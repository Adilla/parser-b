(** Non-empty lists *)

type +'a t
(** Non-empty lists of ['a] *)

val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** [equal ~p l1 l2] tests if two lists are equal.
    Two elements [e1] and [e2] are considered equal when [~p e1 e2] returns [true] *)

val lb_equal : p:('a -> 'b -> bool) -> 'a t -> 'b t -> bool

val make: 'a -> 'a list -> 'a t
(** [make hd tl] creates a non-empty list from [(hd::tl)].*)

val make1: 'a -> 'a t
(** [make hd] creates a non-empty list from [[hd]].*)

val to_list : 'a t -> 'a list
(** [to_list nle] creates a list from [nle]. *)

val from_list : 'a list -> 'a t option
(** [from_list l] creates a non-empty list from [l] when [l] is non-empty. *)

val from_list_exn : 'a list -> 'a t
(** [from_list l] creates a non-empty list from [l] when [l] is non-empty. *)

val cons : 'a -> 'a t -> 'a t
(** [cons hd tl] creates the non-empty list [hd::tl] *)

val hd : 'a t -> 'a
(** [hd nle] returns the head of [nle] *)

val tl : 'a t -> 'a list
(** [tl nle] returns the tail of [nle] *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** map function for non-empty list*)

val rev : 'a t -> 'a t
(** reverse function for non-empty list*)

val lb_map : f:('a -> 'b) -> 'a t -> 'b t

val concat : 'a t -> 'a t -> 'a t

val fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t

val exists : ('a -> bool) -> 'a t -> bool
