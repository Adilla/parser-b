(** Basic logging system *)
val set_verbose : bool -> unit
(** [set_verbose true] (resp. set_verbose false) enables (resp. disable) the logging system. *)
val write : ('a, out_channel, unit) format -> 'a
(** [write chan msg] writes [msg] in channel [chan] if logging is enabled.*)
