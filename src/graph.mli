type t

type t_vertex = string

type t_stats = {
  components: int;
  machines: int;
  refinements: int;
  implementations: int;
  toplevel_machines: int;
  imported_included_machines: int;
}

val create : unit -> t
val add_component : t -> TSyntax.component -> unit
val get_statistics : t -> t_stats
val get_refinements : t -> t_vertex -> t_vertex list

module Machine : sig
  type t = {
    (*out*)
    sees: t_vertex list;
    includes: t_vertex list;
    (*in*)
    seen: t_vertex list;
    refined: t_vertex option;
    included: t_vertex list;
    imported: t_vertex option;
  }
end

module Refinement : sig
  type t = {
    (*out*)
    refines: t_vertex;
    sees: t_vertex list;
    includes: t_vertex list;
    (*in*)
    refined: t_vertex option;
  }
end

module Implementation : sig
  type t = {
    (*out*)
    refines: t_vertex;
    sees: t_vertex list;
    imports: t_vertex list;
  }
end

type t_vertex_infos =
  | Machine of Machine.t
  | Refinement of Refinement.t
  | Implementation of Implementation.t

val iter : (t_vertex -> t_vertex_infos -> unit) -> t -> unit
