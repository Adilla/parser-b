open Utils
open Component
open Expression

type t_kind = Machine | Refinement of ident | Implementation of ident

type  t_component = {
  name: ident;
  parameters: ident list;
  component_kind: t_kind;
  clause_constraints: (loc*predicate) option;
  clause_sees: (loc*ident list) option;
  clause_includes: (loc*machine_instanciation list) option;
  clause_promotes_included_operation: (loc*ident list) option;
  clause_promotes_imported_operation: (loc*ident list) option;
  clause_full_inclusion: (loc*machine_instanciation list) option; (*extends in machine/refinement*)
  clause_full_importation: (loc*machine_instanciation list) option; (*extends in implementation*)
  clause_uses: (loc*ident list) option;
  clause_sets: (loc*set list) option;
  clause_concrete_constants: (loc*ident list) option;
  clause_abstract_constants: (loc*ident list) option;
  clause_properties: (loc*predicate) option;
  clause_concrete_variables: (loc*ident list) option;
  clause_abstract_variables: (loc*ident list) option;
  clause_invariant: (loc*predicate) option;
  clause_assertions: (loc*predicate list) option;
  clause_initialisation: (loc*Substitution.substitution) option;
  clause_operations: (loc*operation list) option;
  clause_local_operations: (loc*operation list) option;
  clause_values: (loc*(ident*expression) list) option;
  clause_imports: (loc*machine_instanciation list) option;
}

let convert_component : component -> t_component = function
  | Abstract_machine mch ->
    { name = mch.name;
      parameters = mch.parameters;
      component_kind = Machine;
      clause_constraints = mch.clause_constraints;
      clause_sees = mch.clause_sees;
      clause_includes = mch.clause_includes;
      clause_promotes_included_operation = mch.clause_promotes;
      clause_promotes_imported_operation = None;
      clause_full_inclusion = mch.clause_extends;
      clause_full_importation = None;
      clause_uses = mch.clause_uses;
      clause_sets = mch.clause_sets;
      clause_concrete_constants = mch.clause_concrete_constants;
      clause_abstract_constants = mch.clause_abstract_constants;
      clause_properties = mch.clause_properties;
      clause_concrete_variables = mch.clause_concrete_variables;
      clause_abstract_variables = mch.clause_abstract_variables;
      clause_invariant = mch.clause_invariant;
      clause_assertions = mch.clause_assertions;
      clause_initialisation = mch.clause_initialisation;
      clause_operations = mch.clause_operations;
      clause_local_operations = None;
      clause_values = None;
      clause_imports = None; }
  | Refinement ref ->
    { name = ref.name;
      parameters = ref.parameters;
      component_kind = Refinement ref.refines;
      clause_constraints = None;
      clause_sees = ref.clause_sees;
      clause_includes = ref.clause_includes;
      clause_promotes_included_operation = ref.clause_promotes;
      clause_promotes_imported_operation = None;
      clause_full_inclusion = None; (*FIXME*)
      clause_full_importation = None;
      clause_uses = None;
      clause_sets = ref.clause_sets;
      clause_concrete_constants = ref.clause_concrete_constants;
      clause_abstract_constants = ref.clause_abstract_constants;
      clause_properties = ref.clause_properties;
      clause_concrete_variables = ref.clause_concrete_variables;
      clause_abstract_variables = ref.clause_abstract_variables;
      clause_invariant = ref.clause_invariant;
      clause_assertions = ref.clause_assertions;
      clause_initialisation = ref.clause_initialisation;
      clause_operations = ref.clause_operations;
      clause_local_operations = None;
      clause_values = None;
      clause_imports = None; }
  | Implementation imp ->
    { name = imp.name;
      parameters = imp.parameters;
      component_kind = Implementation imp.refines;
      clause_constraints = None;
      clause_sees = imp.clause_sees;
      clause_includes = None;
      clause_promotes_included_operation = None;
      clause_promotes_imported_operation = imp.clause_promotes;
      clause_full_inclusion = None;
      clause_full_importation = imp.clause_extends_B0;
      clause_uses = None;
      clause_sets = imp.clause_sets;
      clause_concrete_constants = imp.clause_concrete_constants;
      clause_abstract_constants = None;
      clause_properties = imp.clause_properties;
      clause_concrete_variables = imp.clause_concrete_variables;
      clause_abstract_variables = None;
      clause_invariant = imp.clause_invariant;
      clause_assertions = imp.clause_assertions;
      clause_initialisation = imp.clause_initialisation_B0;
      clause_operations = imp.clause_operations_B0;
      clause_local_operations = imp.clause_local_operations_B0;
      clause_values = imp.clause_values;
      clause_imports = imp.clause_imports; }

type dep_kind = D_Sees | D_Uses | D_Includes | D_Imports
type exp_source = Declared | Included_From of ident | Inherited_E
type exp_source_op = Declared_Operation | Promoted_From of ident
type vis_source = Seen_From of ident | Used_From of ident | Imported_From of ident | Inherited
type vis_source_op = Op_Seen_From of ident | Op_Imported_From of ident | Op_Included_From of ident | Local_Op

type component_view = {
  name: ident;
  parameters: ident list;
  component_kind: t_kind;
  dependencies: (ident*dep_kind) list;
  refined_by: ident list;
  required_by: (ident*dep_kind) list;
  (* Sets *)
  exported_sets: (set*exp_source) list; 
  visible_sets: (set*vis_source) list;
  (* Constants *)
  exported_abstract_constants: (ident*exp_source) list;
  exported_concrete_constants: (ident*exp_source) list;
  visible_abstract_constants:  (ident*vis_source) list;
  visible_concrete_constants:  (ident*vis_source) list;
  (* Variables *)
  exported_abstract_variables: (ident*exp_source) list; 
  exported_concrete_variables: (ident*exp_source) list;
  visible_abstract_variables:  (ident*vis_source) list;
  visible_concrete_variables:  (ident*vis_source) list;
  (* Operations *)
  exported_operations: (ident*exp_source_op) list;
  visible_operations:  (ident*vis_source_op) list;
}

let get_list : ('a*'b list) option -> 'b list = function
  | None -> []
  | Some (_,lst) -> lst

let get_dependencies (mch:t_component) : (ident*dep_kind) list =
  let accu = [] in
  let accu = List.fold_left (fun accu x -> (x,D_Sees)::accu) accu (get_list mch.clause_sees) in
  let accu = List.fold_left (fun accu x -> (x,D_Uses)::accu) accu (get_list mch.clause_uses) in
  let accu = List.fold_left (fun accu (x,_) -> (x,D_Includes)::accu) accu (get_list mch.clause_includes) in
  let accu = List.fold_left (fun accu (x,_) -> (x,D_Includes)::accu) accu (get_list mch.clause_full_inclusion) in
  let accu = List.fold_left (fun accu (x,_) -> (x,D_Imports)::accu) accu (get_list mch.clause_imports) in
  let accu = List.fold_left (fun accu (x,_) -> (x,D_Imports)::accu) accu (get_list mch.clause_full_importation) in
  List.rev accu

let warning_cannot_find_component s =
  Printf.fprintf stdout "Warning: cannot find component '%s'.\n" s

let warning s =
  Printf.fprintf stdout "Warning: %s\n" s

let extract_entities_from_machine_list
    (type a) (type b) (type c)
    (f:string -> component_view option)
    (get:component_view -> (a*c) list)
    (machines:ident list)
    (mk_elt:ident -> a -> b)
  : b list
  =
    List.fold_left (
      fun accu mch_name ->
         match f (snd mch_name) with
           | None -> (warning_cannot_find_component (snd mch_name); accu)
           | Some mch_view ->
           List.fold_left (fun accu (x,_) -> (mk_elt mch_name x)::accu) accu (get mch_view)
      )
        [] machines
(*
let find_declared_ident (lst:(ident*'b) list) (x:ident) : bool =
  List.exists (fun (id,_) -> Utils.ident_eq x id) lst

let is_homonym_ref f get (x:ident) : t_kind -> bool = function
  | Machine -> false
  | Implementation ref | Refinement ref ->
    begin match f (snd ref) with
      | None -> (warning_cannot_find_component (snd ref); false)
      | Some cv -> find_declared_ident (get cv) x
    end

let is_homonym_imp f get x lst : bool =
  let aux imp =
    match f (snd imp) with
    | None -> (warning_cannot_find_component (snd imp); false)
    | Some cv -> find_declared_ident (get cv) x
  in
  List.exists aux lst
   *)

let get_exported_idents
    (f:string -> component_view option)
    (get_mch_idents:component_view -> (ident*'b) list)
    (declared_idents: ident list)
    (included_machines: ident list)
    (fully_included_machines: ident list)
    (ckind: t_kind)
    (inherited:bool)
    (imported:ident list)
  : (ident*exp_source) list
  =
  let lst1 = List.map (fun x -> (x,Declared)) declared_idents in
  let lst2 = extract_entities_from_machine_list f get_mch_idents included_machines
      (fun id x -> (x,Included_From id))
  in
  let lst3 = extract_entities_from_machine_list f get_mch_idents fully_included_machines
      (fun id x -> (x,Included_From id))
  in
  let lst4 = if inherited then
      match ckind with
      | Implementation ref | Refinement ref ->
        begin match f (snd ref) with
          | None -> ( warning_cannot_find_component (snd ref); [] )
          | Some cv -> List.map (fun (x,_) -> (x,Inherited_E)) (get_mch_idents cv)
        end
      | _ -> []
    else []
  in
  List.concat [lst1;lst2;lst3;lst4]

(*FIXME est ce qu'un raffinement ou une implantation hérite de certains ensembles de la machine raffinée? *)
let get_exported_sets (f:string -> component_view option) (mch:t_component) : (set*exp_source) list =
  let get_mch_idents = (fun view -> view.exported_sets) in
  let declared_idents = (get_list mch.clause_sets) in
  let included_machines = (List.map fst (get_list mch.clause_includes)) in
  let fully_included_machines = (List.map fst (get_list mch.clause_full_inclusion)) in
  let lst1 = List.map (fun x -> (x,Declared)) declared_idents in
  let lst2 = extract_entities_from_machine_list f get_mch_idents included_machines
      (fun id x -> (x,Included_From id))
  in
  let lst3 = extract_entities_from_machine_list f get_mch_idents fully_included_machines
      (fun id x -> (x,Included_From id))
  in
  List.concat [lst1;lst2;lst3]

let get_imported_mch (mch:t_component) : ident list =
  match mch.clause_imports with
  | None -> []
  | Some (_,lst) -> List.map fst lst

let get_exported_abstract_constants (f:string -> component_view option) (mch:t_component) : (ident*exp_source) list =
  get_exported_idents f
    (fun view -> view.exported_abstract_constants)
    (get_list mch.clause_abstract_constants)
    (List.map fst (get_list mch.clause_includes))
    (List.map fst (get_list mch.clause_full_inclusion))
    mch.component_kind
    false
    (get_imported_mch mch)

let get_exported_concrete_constants (f:string -> component_view option) (mch:t_component) : (ident*exp_source) list =
  get_exported_idents f
    (fun view -> view.exported_concrete_constants)
    (get_list mch.clause_concrete_constants)
    (List.map fst (get_list mch.clause_includes))
    (List.map fst (get_list mch.clause_full_inclusion))
    mch.component_kind
    true
    (get_imported_mch mch)

let get_exported_abstract_variables (f:string -> component_view option) (mch:t_component) : (ident*exp_source) list =
  get_exported_idents f
    (fun view -> view.exported_abstract_variables)
    (get_list mch.clause_abstract_variables)
    (List.map fst (get_list mch.clause_includes))
    (List.map fst (get_list mch.clause_full_inclusion))
    mch.component_kind
    false
    (get_imported_mch mch)

let get_exported_concrete_variables (f:string -> component_view option) (mch:t_component) : (ident*exp_source) list =
  get_exported_idents f
    (fun view -> view.exported_concrete_variables)
    (get_list mch.clause_concrete_variables)
    (List.map fst (get_list mch.clause_includes))
    (List.map fst (get_list mch.clause_full_inclusion))
    mch.component_kind
    true
    (get_imported_mch mch)

let get_visible_idents
    (type a) (type b)
    (f:string -> component_view option)
    (get_mch_idents:component_view -> (a*b) list)
    (seen_machines: ident list)
    (used_machines: ident list)
    (imported_machines: ident list)
    (fully_imported_machines: ident list)
    (refined_machine: ident option)
  : (a*vis_source) list
  =
  let lst1 = extract_entities_from_machine_list f get_mch_idents seen_machines
      (fun id x -> (x,Seen_From id))
  in
  let lst2 = extract_entities_from_machine_list f get_mch_idents used_machines
      (fun id x -> (x,Used_From id))
  in
  let lst3 = extract_entities_from_machine_list f get_mch_idents imported_machines
      (fun id x -> (x,Imported_From id))
  in
  let lst4 = extract_entities_from_machine_list f get_mch_idents fully_imported_machines
      (fun id x -> (x,Imported_From id))
  in
  let lst5 = match refined_machine with
    | None -> []
    | Some abs -> extract_entities_from_machine_list
                    f get_mch_idents [abs] (fun id x -> (x,Inherited))
  in
  List.concat [lst1;lst2;lst3;lst4;lst5]

let get_visible_sets (f:string -> component_view option) (mch:t_component) : (set*vis_source) list =
  get_visible_idents f
    (fun view -> view.exported_sets)
    (get_list mch.clause_sees)
    (get_list mch.clause_uses)
    (List.map fst (get_list mch.clause_imports))
    (List.map fst (get_list mch.clause_full_importation))
    (match mch.component_kind with
     | Machine -> None
     | Refinement abs | Implementation abs -> Some abs)

let get_visible_abstract_constants  (f:string -> component_view option) (mch:t_component) : (ident*vis_source) list =
  get_visible_idents f
    (fun view -> view.exported_abstract_constants)
    (get_list mch.clause_sees)
    (get_list mch.clause_uses)
    (List.map fst (get_list mch.clause_imports))
    (List.map fst (get_list mch.clause_full_importation))
    (match mch.component_kind with
     | Machine -> None
     | Refinement abs | Implementation abs -> Some abs)

let get_visible_concrete_constants  (f:string -> component_view option) (mch:t_component) : (ident*vis_source) list =
  get_visible_idents f
    (fun view -> view.exported_concrete_constants)
    (get_list mch.clause_sees)
    (get_list mch.clause_uses)
    (List.map fst (get_list mch.clause_imports))
    (List.map fst (get_list mch.clause_full_importation))
    (match mch.component_kind with
     | Machine -> None
     | Refinement abs | Implementation abs -> Some abs)


let get_visible_abstract_variables  (f:string -> component_view option) (mch:t_component) : (ident*vis_source) list =
  get_visible_idents f
    (fun view -> view.exported_abstract_variables)
    (get_list mch.clause_sees)
    (get_list mch.clause_uses)
    (List.map fst (get_list mch.clause_imports))
    (List.map fst (get_list mch.clause_full_importation))
    (match mch.component_kind with
     | Machine -> None
     | Refinement abs | Implementation abs -> Some abs)

let get_visible_concrete_variables  (f:string -> component_view option) (mch:t_component) : (ident*vis_source) list =
  get_visible_idents f
    (fun view -> view.exported_concrete_variables)
    (get_list mch.clause_sees)
    (get_list mch.clause_uses)
    (List.map fst (get_list mch.clause_imports))
    (List.map fst (get_list mch.clause_full_importation))
    (match mch.component_kind with
     | Machine -> None
     | Refinement abs | Implementation abs -> Some abs)

let find_source (f:string -> component_view option) (lst:(ident*'e) list) (op:ident) : ident =
  let rec aux = function
    | [] -> ( warning ("cannot find operation '"^snd op^"'."); (dloc,"???") )
    | (mch_name,_)::tl ->
      begin match f (snd mch_name) with
        | None -> (warning_cannot_find_component (snd mch_name); aux tl)
        | Some view ->
          if List.exists (fun (exported_op,_) -> String.equal (snd op) (snd exported_op)) view.exported_operations then mch_name
          else aux tl
      end
  in
  aux lst

let get_exported_operations (f:string -> component_view option) (mch:t_component) : (ident*exp_source_op) list =
  let lst1 = List.map (fun (_,op,_,_) -> (op,Declared_Operation)) (get_list mch.clause_operations) in
  let local_ops = List.map (fun (_,(_,s),_,_) -> s) (get_list mch.clause_local_operations) in
  let lst1 = List.filter (fun ((_,s),_) -> not (List.mem s local_ops)) lst1 in
  let lst2 = List.map (fun x -> (x,Promoted_From (find_source f (get_list mch.clause_includes) x))) (get_list mch.clause_promotes_included_operation) in
  let lst3 = List.map (fun x -> (x,Promoted_From (find_source f (get_list mch.clause_imports) x))) (get_list mch.clause_promotes_imported_operation) in
  let get_exp_ops (inc_mch,_) : (ident*exp_source_op) list =
    match f (snd inc_mch) with
    | None -> (warning_cannot_find_component (snd inc_mch); [])
    | Some view -> List.map (fun (op,_) -> (op,Promoted_From inc_mch)) view.exported_operations
  in
  let lst4 =  List.concat (List.map get_exp_ops (get_list mch.clause_full_inclusion)) in
  let lst5 =  List.concat (List.map get_exp_ops (get_list mch.clause_full_importation)) in
  List.concat [ lst1; lst2; lst3; lst4; lst5 ]

let get_visible_operations (f:string -> component_view option) (mch:t_component) : (ident*vis_source_op) list =
  let get_exp_ops (s:ident -> vis_source_op) (mch2:ident) : (ident*vis_source_op) list =
    match f (snd mch2) with
    | None -> (warning_cannot_find_component (snd mch2); [])
    | Some view -> List.map (fun (op,_) -> (op,s mch2)) view.exported_operations
  in
  let lst1 = List.concat (List.map (get_exp_ops (fun x -> Op_Seen_From x)) (get_list mch.clause_sees)) in
  let lst2 = List.concat (List.map (get_exp_ops (fun x -> Op_Included_From x)) (List.map fst (get_list mch.clause_includes))) in
  let lst3 = List.concat (List.map (get_exp_ops (fun x -> Op_Imported_From x)) (List.map fst (get_list mch.clause_imports))) in
  let lst4 = List.map (fun (_,op,_,_) -> (op,Local_Op)) (get_list mch.clause_local_operations) in
  List.concat [lst1;lst2;lst3;lst4]

let make (f:string -> component_view option) (refined_by:ident list)
    (required_by:(ident*dep_kind) list) (component:component) : component_view =
  let mch = convert_component component in
  { name = mch.name;
    parameters = mch.parameters;
    component_kind = mch.component_kind;
    dependencies = get_dependencies mch;
    (* backrefs *)
    refined_by = refined_by;
    required_by = required_by;
    (* Sets *)
    exported_sets = get_exported_sets f mch;
    visible_sets = get_visible_sets f mch;
    (* Constants *)
    exported_abstract_constants = get_exported_abstract_constants f mch;
    exported_concrete_constants = get_exported_concrete_constants f mch;
    visible_abstract_constants = get_visible_abstract_constants f mch;
    visible_concrete_constants = get_visible_concrete_constants f mch;
    (* Variables *)
    exported_abstract_variables = get_exported_abstract_variables f mch;
    exported_concrete_variables = get_exported_concrete_variables f mch;
    visible_abstract_variables = get_visible_abstract_variables f mch;
    visible_concrete_variables = get_visible_concrete_variables f mch;
    (* Operations *)
    exported_operations = get_exported_operations f mch;
    visible_operations = get_visible_operations f mch;
  }
