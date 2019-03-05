type t_vertex = string

module Root = struct
  type t = {
    (*out*)
    sees: t_vertex list;
    includes: t_vertex list;
    (*in*)
    seen: t_vertex list;
    refined: t_vertex option;
  }
end

module Imported = struct
  type t = {
    (*out*)
    sees: t_vertex list;
    includes: t_vertex list;
    (*in*)
    seen: t_vertex list;
    refined: t_vertex option;
    imported: t_vertex option;
  }
end

module Included = struct
  type t = {
    (*out*)
    sees: t_vertex list;
    includes: t_vertex list;
    (*in*)
    included: t_vertex list;
  }
end

module Refinement = struct
  type t = {
    (*out*)
    refines: t_vertex;
    sees: t_vertex list;
    includes: t_vertex list;
    (*in*)
    refined: t_vertex option;
  }
end

module Implementation = struct
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

type t = (string,t_vertex_infos) Hashtbl.t

(*
type t_edge = {
  edge_from: t_vertex;
  edge_to: t_vertex;
  edge_kind: t_edge_kind
}
*)

let create () = Hashtbl.create 47

let set_seen_machine (graph:t) (sees:string) (seen:SyntaxCore.lident) : unit =
  match Hashtbl.find_opt graph seen.lid_str with
  | None -> assert false (*FIXME*)
  | Some (RootMachine infos) ->
    Hashtbl.replace graph seen.lid_str (RootMachine { infos with seen=sees::infos.seen })
  | Some (ImportedMachine infos) ->
    Hashtbl.replace graph seen.lid_str (ImportedMachine { infos with seen=sees::infos.seen })
  | Some (IncludedMachine _) -> assert false
  | Some (Refinement _) -> assert false
  | Some (Implementation _) -> assert false

let set_included_machine (graph:t) (included_by:string) (included:SyntaxCore.lident) : unit =
  match Hashtbl.find_opt graph included.lid_str with
  | None -> assert false (*FIXME*)
  | Some (RootMachine {sees;includes;seen;refined}) ->
    begin match seen, refined with
      | _::_, _ -> assert false (*FIXME*)
      | _, Some _ -> assert false (*FIXME*)
      | [], None -> Hashtbl.replace graph included.lid_str
                    (IncludedMachine {  sees; includes; included=[included_by] })
    end
  | Some (IncludedMachine infos) ->
    Hashtbl.replace graph included.lid_str
      (IncludedMachine { infos with included=included_by::infos.included })
  | Some (ImportedMachine _) -> assert false
  | Some (Refinement _) -> assert false
  | Some (Implementation _) -> assert false

let set_refined_machine (graph:t) (refined_by:string) (refined:SyntaxCore.lident) : unit =
  match Hashtbl.find_opt graph refined.lid_str with
  | None -> assert false (*FIXME*)
  | Some (RootMachine infos) ->
    begin match infos.refined with
      | Some _ -> assert false (*FIXME*)
      | None -> Hashtbl.replace graph refined.lid_str
                  (RootMachine { infos with refined=Some refined_by })
    end
  | Some (ImportedMachine infos) ->
    begin match infos.refined with
      | Some _ -> assert false (*FIXME*)
      | None -> Hashtbl.replace graph refined.lid_str
                  (ImportedMachine { infos with refined=Some refined_by })
    end
  | Some (Refinement infos) ->
    begin match infos.refined with
      | Some _ -> assert false (*FIXME*)
      | None -> Hashtbl.replace graph refined.lid_str
                  (Refinement { infos with refined=Some refined_by })
    end
  | Some (IncludedMachine _) -> assert false
  | Some (Implementation _) -> assert false

let set_imported_machine (graph:t) (imported_by:string) (imported:SyntaxCore.lident) : unit =
  match Hashtbl.find_opt graph imported.lid_str with
  | None -> assert false (*FIXME*)
  | Some (RootMachine {sees;includes;seen;refined}) ->
    Hashtbl.replace graph imported.lid_str
      (ImportedMachine {  sees; includes; seen; refined; imported=Some imported_by })
  | Some (ImportedMachine infos) ->
    begin match infos.imported with
      | Some _ -> assert false (*FIXME*)
      | None ->
        Hashtbl.replace graph imported.lid_str
          (ImportedMachine { infos with imported=Some imported_by })
    end
  | Some (IncludedMachine _) -> assert false (*FIXME*)
  | Some (Refinement _) -> assert false
  | Some (Implementation _) -> assert false

let add_component (graph:t) (vertex:TSyntax.component) : unit =
  match Hashtbl.find_opt graph vertex.co_name.lid_str with
  | None ->
    begin match vertex.co_desc with
      | Machine mch ->
        begin
          Hashtbl.add graph vertex.co_name.lid_str
            (RootMachine {
                sees =  List.map (fun lid -> lid.SyntaxCore.lid_str) mch.mch_sees;
                includes =
                  (List.map (fun lid -> lid.SyntaxCore.lid_str) mch.mch_includes)@
                  (List.map (fun lid -> lid.SyntaxCore.lid_str) mch.mch_extends);
                seen = [];
                refined = None;
              });
          List.iter (set_seen_machine graph vertex.co_name.lid_str) mch.mch_sees;
          List.iter (set_included_machine graph vertex.co_name.lid_str) mch.mch_includes;
          List.iter (set_included_machine graph vertex.co_name.lid_str) mch.mch_extends
        end
      | Refinement ref ->
        begin
          Hashtbl.add graph vertex.co_name.lid_str
            (Refinement {
                refines = ref.ref_refines.lid_str;
                sees =  List.map (fun lid -> lid.SyntaxCore.lid_str) ref.ref_sees;
                includes =
                  (List.map (fun lid -> lid.SyntaxCore.lid_str) ref.ref_includes)@
                  (List.map (fun lid -> lid.SyntaxCore.lid_str) ref.ref_extends);
                refined = None;
              });
          set_refined_machine graph vertex.co_name.lid_str ref.ref_refines;
          List.iter (set_seen_machine graph vertex.co_name.lid_str) ref.ref_sees;
          List.iter (set_included_machine graph vertex.co_name.lid_str) ref.ref_includes;
          List.iter (set_included_machine graph vertex.co_name.lid_str) ref.ref_extends
        end
      | Implementation imp ->
        begin
          Hashtbl.add graph vertex.co_name.lid_str
            (Implementation {
                refines = imp.imp_refines.lid_str;
                sees =  List.map (fun lid -> lid.SyntaxCore.lid_str) imp.imp_sees;
                imports =
                  (List.map (fun lid -> lid.SyntaxCore.lid_str) imp.imp_imports)@
                  (List.map (fun lid -> lid.SyntaxCore.lid_str) imp.imp_extends);
              });
          set_refined_machine graph vertex.co_name.lid_str imp.imp_refines;
          List.iter (set_seen_machine graph vertex.co_name.lid_str) imp.imp_sees;
          List.iter (set_imported_machine graph vertex.co_name.lid_str) imp.imp_imports;
          List.iter (set_imported_machine graph vertex.co_name.lid_str) imp.imp_extends
        end
    end
  | Some _ -> assert false (*FIXME*)

(* 
 * on veut connaitre les machines qui ne sont pas importées ou incluses
 * les machine incluses ne sont ni importées, ni raffinées, ni vu
 * les machines sont importées au plus une fois
 * pas de cycle
 * regle sur les sees
 *)
