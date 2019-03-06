type t

type t_vertex = string

type t_stats = {
  components: int;
  machines: int;
  refinements: int;
  implementations: int;
  toplevel_machines: int;
  toplevel_base_machines: int;
  included_machines: int;
  imported_machines: int;
  imported_base_machines: int;
}

val create : unit -> t
val add_component : t -> TSyntax.component -> unit
val get_statistics : t -> t_stats
val get_refinements : t -> t_vertex -> t_vertex list

module Root : sig
  type t = {
    (*out*)
    sees: t_vertex list;
    includes: t_vertex list;
    (*in*)
    seen: t_vertex list;
    refined: t_vertex option;
  }
end

module Imported : sig
  type t = {
    (*out*)
    sees: t_vertex list;
    includes: t_vertex list;
    (*in*)
    seen: t_vertex list;
    refined: t_vertex option;
    imported: t_vertex;
  }
end

module Included : sig
  type t = {
    (*out*)
    sees: t_vertex list;
    includes: t_vertex list;
    (*in*)
    included: t_vertex list;
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
  | RootMachine of Root.t
  | ImportedMachine of Imported.t
  | IncludedMachine of Included.t
  | Refinement of Refinement.t
  | Implementation of Implementation.t

val iter : (t_vertex -> t_vertex_infos -> unit) -> t -> unit
