type 'a t
val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val make: 'a -> 'a list -> 'a t
val make1: 'a -> 'a t
val to_list : 'a t -> 'a list
val from_list : 'a list -> 'a t option
val from_list_exn : 'a list -> 'a t
val cons : 'a -> 'a t -> 'a t
val hd : 'a t -> 'a
val tl : 'a t -> 'a list
val map : ('a -> 'b) -> 'a t -> 'b t
