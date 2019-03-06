type t

type t_vertex = string

type t_stats = {
  components: int;
  machines: int;
  refinements: int;
  implementations: int;
  toplevel_machines: int;
  imported_included_machines: int;
  missing_machines: int;
}

val create : unit -> t
val add_component : t -> PSyntax.component -> unit
val get_statistics : t -> t_stats
