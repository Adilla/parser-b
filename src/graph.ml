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

module Missing = struct
  type t = {
    (*in*)
    seen: t_vertex list;
    refined: t_vertex option;
    included: t_vertex list;
    imported: t_vertex option;
  }
end

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
  | Missing of Missing.t

type t = (string,t_vertex_infos) Hashtbl.t

let create () = Hashtbl.create 47

let set_seen_machine (graph:t) (sees:string) (seen:SyntaxCore.lident) : unit =
  match Hashtbl.find_opt graph seen.lid_str with
  | None ->
    Hashtbl.add graph seen.lid_str
      (Missing { seen=[sees]; refined=None; imported=None; included=[] })
  | Some (Machine infos) ->
    Hashtbl.replace graph seen.lid_str (Machine { infos with seen=sees::infos.seen })
  | Some (Missing infos) ->
    Hashtbl.replace graph seen.lid_str (Missing { infos with seen=sees::infos.seen })
  | Some (Refinement _) ->
    Error.raise_exn seen.lid_loc "Only machines can be seen. This component is a refinement."
  | Some (Implementation _) ->
    Error.raise_exn seen.lid_loc "Only machines can be seen. This component is an implementation."

let set_included_machine (graph:t) (included_by:string) (included:SyntaxCore.lident) : unit =
  match Hashtbl.find_opt graph included.lid_str with
  | None ->
    Hashtbl.add graph included.lid_str
      (Missing { seen=[]; refined=None; imported=None; included=[included_by] })
  | Some (Machine infos) ->
    Hashtbl.replace graph included.lid_str
      (Machine { infos with included=included_by::infos.included })
  | Some (Missing infos) ->
    Hashtbl.replace graph included.lid_str
      (Missing { infos with included=included_by::infos.included })
  | Some (Refinement _) ->
    Error.raise_exn included.lid_loc "Only machines can be included. This component is a refinement."
  | Some (Implementation _) ->
    Error.raise_exn included.lid_loc "Only machines can be included. This component is an implementation."

let set_refined_machine (graph:t) (refined_by:string) (refined:SyntaxCore.lident) : unit =
  match Hashtbl.find_opt graph refined.lid_str with
  | None ->
    Hashtbl.add graph refined.lid_str
      (Missing { seen=[]; refined=Some refined_by; imported=None; included=[] })
  | Some (Machine infos) ->
    begin match infos.refined with
      | Some refined_by_2 ->
          Error.raise_exn refined.lid_loc
            ("The machine '"^refined.lid_str^"' is refined twice; by '" ^ refined_by_2^"' and by '"^refined_by^"'.")
(*
          Hashtbl.replace graph refined.lid_str
            (Machine { infos with refined=Some refined_by })
*)
      | None -> Hashtbl.replace graph refined.lid_str
                  (Machine { infos with refined=Some refined_by })
    end
  | Some (Refinement infos) ->
    begin match infos.refined with
      | Some refined_by_2 ->
          Error.raise_exn refined.lid_loc
              ("The refinement '"^refined.lid_str^"' is refined twice; by '" ^refined_by_2^"' and by '"^refined_by^"'.")
      | None -> Hashtbl.replace graph refined.lid_str
                  (Refinement { infos with refined=Some refined_by })
    end
  | Some (Missing infos) ->
    begin match infos.refined with
      | Some refined_by_2 ->
          Error.raise_exn refined.lid_loc
            ("The machine '"^refined.lid_str^"' is refined twice; by '"
             ^refined_by_2^"' and by '"^refined_by^"'.")
      | None -> Hashtbl.replace graph refined.lid_str
                  (Missing { infos with refined=Some refined_by })
    end
  | Some (Implementation _) ->
    Error.raise_exn refined.lid_loc "Implementations cannot be refined."

let set_imported_machine (graph:t) (imported_by:string) (imported:SyntaxCore.lident) : unit =
  match Hashtbl.find_opt graph imported.lid_str with
  | None ->
    Hashtbl.add graph imported.lid_str
      (Missing { seen=[]; imported=Some imported_by; refined=None; included=[] })
  | Some (Machine infos) ->
    begin match infos.imported with
      | Some _ -> () (*FIXME
        Error.raise_exn imported.lid_loc
          ("The machine '"^imported.lid_str^"' is imported twice; by '"
             ^imported_by_2^"' and by '"^imported_by^"'.")
                                   *)
      | None -> Hashtbl.replace graph imported.lid_str
                  (Machine { infos with imported=Some imported_by })
    end
  | Some (Missing infos) ->
    begin match infos.imported with
      | Some _ ->() (*FIXME

        Error.raise_exn imported.lid_loc
          ("The machine '"^imported.lid_str^"' is imported twice; by '"
             ^imported_by_2^"' and by '"^imported_by^"'.")
                                  *)
      | None -> Hashtbl.replace graph imported.lid_str
                  (Missing { infos with imported=Some imported_by })
    end
  | Some (Refinement _) ->
    Error.raise_exn imported.lid_loc "Only machines can be imported. This component is a refinement."
  | Some (Implementation _) ->
    Error.raise_exn imported.lid_loc "Only machines can be imported. This component is an implementation."

let get_machine_name ma =
  match ma.PSyntax.mi_params with
  | [] -> ma.PSyntax.mi_mch.SyntaxCore.lid_str
  | _::_ ->
    Error.raise_exn ma.PSyntax.mi_mch.lid_loc "Not implemented: machines with parameters."

let add_component (graph:t) (vertex:PSyntax.component) : unit =
  match Hashtbl.find_opt graph vertex.co_name.lid_str with
  | None ->
    begin match vertex.co_desc with
      | Machine mch ->
        begin
          Hashtbl.add graph vertex.co_name.lid_str
            (Machine {
                sees =  List.map (fun lid -> lid.SyntaxCore.lid_str) mch.mch_sees;
                includes =
                  (List.map get_machine_name mch.mch_includes)@
                  (List.map get_machine_name mch.mch_extends);
                seen = [];
                refined = None;
                imported = None;
                included = [];
              });
          List.iter (set_seen_machine graph vertex.co_name.lid_str) mch.mch_sees;
          List.iter (fun mi -> set_included_machine graph vertex.co_name.lid_str mi.PSyntax.mi_mch) mch.mch_includes;
          List.iter (fun mi -> set_included_machine graph vertex.co_name.lid_str mi.PSyntax.mi_mch) mch.mch_extends
        end
      | Refinement ref ->
        begin
          Hashtbl.add graph vertex.co_name.lid_str
            (Refinement {
                refines = ref.ref_refines.lid_str;
                sees =  List.map (fun lid -> lid.SyntaxCore.lid_str) ref.ref_sees;
                includes =
                  (List.map get_machine_name ref.ref_includes)@
                  (List.map get_machine_name ref.ref_extends);
                refined = None;
              });
          set_refined_machine graph vertex.co_name.lid_str ref.ref_refines;
          List.iter (set_seen_machine graph vertex.co_name.lid_str) ref.ref_sees;
          List.iter (fun mi -> set_included_machine graph vertex.co_name.lid_str mi.PSyntax.mi_mch) ref.ref_includes;
          List.iter (fun mi -> set_included_machine graph vertex.co_name.lid_str mi.PSyntax.mi_mch) ref.ref_extends
        end
      | Implementation imp ->
        begin
          Hashtbl.add graph vertex.co_name.lid_str
            (Implementation {
                refines = imp.imp_refines.lid_str;
                sees =  List.map (fun lid -> lid.SyntaxCore.lid_str) imp.imp_sees;
                imports =
                  (List.map get_machine_name imp.imp_imports)@
                  (List.map get_machine_name imp.imp_extends);
              });
          set_refined_machine graph vertex.co_name.lid_str imp.imp_refines;
          List.iter (set_seen_machine graph vertex.co_name.lid_str) imp.imp_sees;
          List.iter (fun mi -> set_imported_machine graph vertex.co_name.lid_str mi.PSyntax.mi_mch) imp.imp_imports;
          List.iter (fun mi -> set_imported_machine graph vertex.co_name.lid_str mi.PSyntax.mi_mch) imp.imp_extends
        end
    end
  | Some Missing infos ->
    begin match vertex.co_desc with
      | Machine mch ->
        begin
          Hashtbl.replace graph vertex.co_name.lid_str
            (Machine {
                sees =  List.map (fun lid -> lid.SyntaxCore.lid_str) mch.mch_sees;
                includes =
                  (List.map get_machine_name mch.mch_includes)@
                  (List.map get_machine_name mch.mch_extends);
                seen = infos.seen;
                refined = infos.refined;
                imported = infos.imported;
                included = infos.included;
              });
          List.iter (set_seen_machine graph vertex.co_name.lid_str) mch.mch_sees;
          List.iter (fun mi -> set_included_machine graph vertex.co_name.lid_str mi.PSyntax.mi_mch) mch.mch_includes;
          List.iter (fun mi -> set_included_machine graph vertex.co_name.lid_str mi.PSyntax.mi_mch) mch.mch_extends
        end
        | Refinement ref ->
          begin
            (if infos.imported != None then
               Error.raise_exn vertex.co_name.lid_loc "Refinement cannot be imported.");
            (if infos.included != [] then
               Error.raise_exn vertex.co_name.lid_loc "Refinement cannot be included.");
            (if infos.seen != [] then
               Error.raise_exn vertex.co_name.lid_loc "Refinement cannot be seen.");
            Hashtbl.replace graph vertex.co_name.lid_str
              (Refinement {
                  refines = ref.ref_refines.lid_str;
                  sees =  List.map (fun lid -> lid.SyntaxCore.lid_str) ref.ref_sees;
                  includes =
                    (List.map get_machine_name ref.ref_includes)@
                    (List.map get_machine_name ref.ref_extends);
                  refined = infos.refined;
                });
            set_refined_machine graph vertex.co_name.lid_str ref.ref_refines;
            List.iter (set_seen_machine graph vertex.co_name.lid_str) ref.ref_sees;
            List.iter (fun mi -> set_included_machine graph vertex.co_name.lid_str mi.PSyntax.mi_mch) ref.ref_includes;
            List.iter (fun mi -> set_included_machine graph vertex.co_name.lid_str mi.PSyntax.mi_mch) ref.ref_extends
          end
        | Implementation _ ->
          begin
            (if infos.imported != None then
               Error.raise_exn vertex.co_name.lid_loc "Implementation cannot be imported.");
            (if infos.included != [] then
               Error.raise_exn vertex.co_name.lid_loc "Implementation cannot be included.");
            (if infos.seen != [] then
               Error.raise_exn vertex.co_name.lid_loc "Implementation cannot be seen.");
            (if infos.refined != None then
               Error.raise_exn vertex.co_name.lid_loc "Implementation cannot be refined.");
            assert false;
          end
        end
  | Some _ ->
    Error.raise_exn vertex.co_name.lid_loc "There is already a component with this name."

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
    | Missing _ ->
      { st with missing_machines=st.missing_machines+1 }
  in
  Hashtbl.fold aux graph 
    { components=0; machines=0; refinements=0; implementations=0;
      toplevel_machines=0; imported_included_machines=0; missing_machines=0 }

let rec get_implementation (graph:t) (mch_name:string) : (string*Implementation.t) option =
  match Hashtbl.find_opt graph mch_name with
  | None -> assert false
  | Some Machine { refined=None }
  | Some Refinement { refined=None }
  | Some Missing _ -> None
  | Some Implementation imp -> Some (mch_name,imp)
  | Some Machine { refined=Some ref }
  | Some Refinement { refined=Some ref } -> get_implementation graph ref

let get_implementation_name (graph:t) (mch_name:string) =
  match get_implementation graph mch_name with
  | None -> None
  | Some (x,_) -> Some x 

let get_machines (graph:t) : string list =
  Hashtbl.fold (fun cname infos accu ->
      match infos with
      | Machine { imported=Some _ } | Machine { seen=_::_ } -> cname::accu
      | Machine { imported=None; seen=[]; refined=Some ref } ->
       begin match get_implementation graph ref with
         | Some _ -> cname::accu
         | None ->( accu )
       end
      | Machine { imported=None; seen=[]; refined=None; included=_::_ } ->
        ( Printf.fprintf stdout "Skipping %s (included)\n" cname; accu )
      | Machine { imported=None; seen=[]; refined=None; included=[] } ->
        ( Printf.fprintf stdout "Skipping %s (not used?)\n" cname; accu )
      | Refinement _ | Implementation _ -> accu
      | Missing _ ->
        Error.raise_exn Utils.dloc ("Machine '"^cname^"' is missing")
    ) graph []

module SSet = Set.Make(String)

let queue_to_list queue =
  let rec loop () =
    if Queue.is_empty queue then []
    else
      let hd = Queue.pop queue in
      let tl = loop () in
      hd::tl
  in
  loop ()

let get_sorted_machines (graph:t) : string list =
  let queue = Queue.create () in
  let visited_set = ref SSet.empty in
  let rec visit2 cname imports sees =
    let aux mch = visit mch (Hashtbl.find graph mch) in
    List.iter aux imports;
    List.iter aux sees;
    if not (SSet.mem cname !visited_set) then
      ( Queue.add cname queue;
        visited_set := SSet.add cname !visited_set)
  and visit cname infos =
    match infos with
    | Machine { imported=Some _; sees } | Machine { seen=_::_; sees } ->
      let imports = match get_implementation graph cname with
        | None -> []
        | Some (_,imp) -> imp.imports
      in
      visit2 cname imports sees
    | Machine { imported=None; seen=[]; refined=Some ref; sees } ->
      begin match get_implementation graph ref with
        | Some (_,imp) -> visit2 cname imp.imports sees
        | None -> ()
      end
    | Machine { imported=None; seen=[]; refined=None; included=_::_ } -> ()
    | Machine { imported=None; seen=[]; refined=None; included=[] } -> ()
    | Refinement _ | Implementation _ | Missing _ -> ()
  in
  Hashtbl.iter visit graph; 
  queue_to_list queue
