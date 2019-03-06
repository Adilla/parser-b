type t_vertex = string
type t_stats = {
  components: int;
  machines: int;
  refinements: int;
  implementations: int;
  toplevel_machines: int;
  imported_included_machines: int;
}

module Machine = struct
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
  | Machine of Machine.t
  | Refinement of Refinement.t
  | Implementation of Implementation.t

type t = (string,t_vertex_infos) Hashtbl.t

let create () = Hashtbl.create 47

let set_seen_machine (graph:t) (sees:string) (seen:SyntaxCore.lident) : unit =
  match Hashtbl.find_opt graph seen.lid_str with
  | None -> assert false (*FIXME*)
  | Some (Machine infos) ->
    Hashtbl.replace graph seen.lid_str (Machine { infos with seen=sees::infos.seen })
  | Some (Refinement _) -> assert false
  | Some (Implementation _) -> assert false

let set_included_machine (graph:t) (included_by:string) (included:SyntaxCore.lident) : unit =
  match Hashtbl.find_opt graph included.lid_str with
  | None -> assert false (*FIXME*)
  | Some (Machine infos) ->
    Hashtbl.replace graph included.lid_str
      (Machine { infos with included=included_by::infos.included })
  | Some (Refinement _) -> assert false
  | Some (Implementation _) -> assert false

let set_refined_machine (graph:t) (refined_by:string) (refined:SyntaxCore.lident) : unit =
  match Hashtbl.find_opt graph refined.lid_str with
  | None -> assert false (*FIXME*)
  | Some (Machine infos) ->
    begin match infos.refined with
      | Some refined_by_2 ->
        begin
          Error.print_error
            { Error.err_loc=refined.lid_loc;
              err_txt=("The machine '"^refined.lid_str^"' is refined twice; by '"
                       ^refined_by_2^"' and by '"^refined_by^"'."); };
          Hashtbl.replace graph refined.lid_str
            (Machine { infos with refined=Some refined_by })
        end
      | None -> Hashtbl.replace graph refined.lid_str
                  (Machine { infos with refined=Some refined_by })
    end
  | Some (Refinement infos) ->
    begin match infos.refined with
      | Some _ -> assert false (*FIXME*)
      | None -> Hashtbl.replace graph refined.lid_str
                  (Refinement { infos with refined=Some refined_by })
    end
  | Some (Implementation _) -> assert false

let set_imported_machine (graph:t) (imported_by:string) (imported:SyntaxCore.lident) : unit =
  match Hashtbl.find_opt graph imported.lid_str with
  | None -> assert false (*FIXME*)
  | Some (Machine infos) ->
    begin match infos.imported with
      | Some _ -> assert false (*FIXME*)
      | None -> Hashtbl.replace graph imported.lid_str
                  (Machine { infos with imported=Some imported_by })
    end
  | Some (Refinement _) -> assert false
  | Some (Implementation _) -> assert false

let add_component (graph:t) (vertex:TSyntax.component) : unit =
  match Hashtbl.find_opt graph vertex.co_name.lid_str with
  | None ->
    begin match vertex.co_desc with
      | Machine mch ->
        begin
          Hashtbl.add graph vertex.co_name.lid_str
            (Machine {
                sees =  List.map (fun lid -> lid.SyntaxCore.lid_str) mch.mch_sees;
                includes =
                  (List.map (fun lid -> lid.SyntaxCore.lid_str) mch.mch_includes)@
                  (List.map (fun lid -> lid.SyntaxCore.lid_str) mch.mch_extends);
                seen = [];
                refined = None;
                imported = None;
                included = [];
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

let rec get_refinements (graph:t) (mch_name:t_vertex) : t_vertex list =
  match Hashtbl.find_opt graph mch_name with
  | None -> assert false (*FIXME*)
  | Some Machine { refined=None; _ }
  | Some Refinement { refined=None; _ }
  | Some Implementation _ -> []
  | Some Machine { refined=Some r; _ }
  | Some Refinement { refined=Some r; _ } ->
    r::(get_refinements graph r)

let get_statistics (graph:t) : t_stats =
  let aux _ v st =
    match v with
    | Machine { imported=None; included=[] } ->
      { st with components=st.components+1;
                machines=st.machines+1;
                toplevel_machines=st.toplevel_machines+1 }
    | Machine _ ->
      { st with components=st.components+1;
                machines=st.machines+1;
                imported_included_machines=st.imported_included_machines+1 }
    | Refinement _ ->
      { st with components=st.components+1;
                refinements=st.refinements+1 }
    | Implementation _ ->
      { st with components=st.components+1;
                implementations=st.implementations+1 }
  in
  Hashtbl.fold aux graph 
    { components=0; machines=0; refinements=0; implementations=0;
      toplevel_machines=0; imported_included_machines=0;  }

let iter = Hashtbl.iter
(* 
 * on veut connaitre les machines qui ne sont pas importées ou incluses
 * les machine incluses ne sont ni importées, ni raffinées, ni vu
 * les machines sont importées au plus une fois
 * pas de cycle
 * regle sur les sees
 *)
