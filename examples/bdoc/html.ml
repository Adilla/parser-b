open View
open Utils

let generate_html_header (out:out_channel) : unit =
  Printf.fprintf out "<!DOCTYPE html>\n";
  Printf.fprintf out "<html>\n";
  Printf.fprintf out "<head>\n";
  Printf.fprintf out "<meta charset=\"UTF-8\">\n";
  Printf.fprintf out "<title>List of Machines</title>\n";
  Printf.fprintf out "<style>\na {color:blue; text-decoration: underline;}\na:hover {color: blue; text-decoration: none;}\n</style>\n";
  Printf.fprintf out "</head>\n";
  Printf.fprintf out "<body>\n"

let generate_html_footer (out:out_channel) : unit =
  Printf.fprintf out "</body>\n";
  Printf.fprintf out "</html>\n"

let generate_index (root_dir:string) (machines:(string*bool) list) : unit =
  let out = open_out (root_dir ^ "/index.html") in
  let sorted_machines =
    List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) machines in
  generate_html_header out;
  Printf.fprintf out "<h1> List of Machines</h1>\n";
  Printf.fprintf out "<ul>\n";
  List.iter (fun (mch,is_entry) ->
      if is_entry then
        Printf.fprintf out "<li><a href=\"./machines/%s.html\"><b>%s</b></a></li>\n" mch mch
      else
        Printf.fprintf out "<li><a href=\"./machines/%s.html\">%s</a></li>\n" mch mch
    ) sorted_machines;
  Printf.fprintf out "</ul>\n";
  generate_html_footer out

let pp_kind (out:out_channel) : t_kind -> unit = function
  | Machine -> Printf.fprintf out "Machine"
  | Refinement _ -> Printf.fprintf out "Refinement"
  | Implementation _ -> Printf.fprintf out "Implementation"

let pp_dep_kind (out:out_channel) : dep_kind -> unit = function
  | D_Includes -> Printf.fprintf out " <span style=\"color:red\">[Included]</span>"
  | D_Sees -> Printf.fprintf out " <span style=\"color:red\">[Seen]</span>"
  | D_Imports -> Printf.fprintf out " <span style=\"color:red\">[Imported]</span>"
  | D_Uses -> Printf.fprintf out " <span style=\"color:red\">[Used]</span>"

let pp_exp_source (out:out_channel) : exp_source -> unit = function
  | Declared -> ()
  | Included_From mch -> Printf.fprintf out " <span style=\"color:red\">[Included from <a href=\"./%s.html\">%s</a>]</span>" (snd mch) (snd mch)
  | Inherited_E -> Printf.fprintf out " <span style=\"color:red\">[Inherited]</span>"

let pp_exp_source_op (out:out_channel) : exp_source_op -> unit = function
  | Declared_Operation -> ()
  | Promoted_From mch -> Printf.fprintf out " <span style=\"color:red\">[Included from <a href=\"./%s.html\">%s</a>]</span>" (snd mch) (snd mch)

let pp_vis_source (out:out_channel) : vis_source -> unit = function
  | Seen_From mch -> Printf.fprintf out " <span style=\"color:red\">[Seen from <a href=\"./%s.html\">%s</a>]</span>" (snd mch) (snd mch)
  | Used_From mch -> Printf.fprintf out " <span style=\"color:red\">[Used from <a href=\"./%s.html\">%s</a>]</span>" (snd mch) (snd mch)
  | Imported_From mch -> Printf.fprintf out " <span style=\"color:red\">[Imported from <a href=\"./%s.html\">%s</a>]</span>" (snd mch) (snd mch)
  | Inherited -> Printf.fprintf out " <span style=\"color:red\">[Inherited]</span>"

let pp_vis_source_op (out:out_channel) : vis_source_op -> unit = function
  | Op_Seen_From mch -> Printf.fprintf out " <span style=\"color:red\">[Seen from <a href=\"./%s.html\">%s</a>]</span>" (snd mch) (snd mch)
  | Op_Imported_From mch -> Printf.fprintf out " <span style=\"color:red\">[Imported from <a href=\"./%s.html\">%s</a>]</span>" (snd mch) (snd mch)
  | Op_Included_From mch -> Printf.fprintf out " <span style=\"color:red\">[Included/Promoted from <a href=\"./%s.html\">%s</a>]</span>" (snd mch) (snd mch)
  | Local_Op -> Printf.fprintf out " <span style=\"color:red\">[Local Operation]</span>"

let pp_ident (out:out_channel) (id:u_ident) : unit = Printf.fprintf out "%s" (snd id)

let pp_dep (out:out_channel) (name,kind:u_ident*dep_kind) : unit =
  Printf.fprintf out "<a href=\"./%a.html\">%a</a>%a"
    pp_ident name pp_ident name pp_dep_kind kind

let pp_refined_by (out:out_channel) (name:u_ident) : unit =
  Printf.fprintf out "<a href=\"./%a.html\">%a</a>"
    pp_ident name pp_ident name

let pp_ident_list (out:out_channel) (lst:u_ident list) : unit =
  match lst with
  | [] -> ()
  | hd::tl ->
    begin
      Printf.fprintf out "%a" pp_ident hd;
      List.iter (fun x -> Printf.fprintf out ", %a" pp_ident x) tl
    end

let pp_exported_set (out:out_channel) (set,src:loc Component.set*exp_source) : unit =
  match set with
  | Component.Abstract_Set id ->
    Printf.fprintf out "%a%a" pp_ident id pp_exp_source src
  | Component.Concrete_Set (id,elts) ->
    begin
      Printf.fprintf out "%a%a\n" pp_ident id pp_exp_source src;
      Printf.fprintf out "<ul>\n";
      List.iter (fun id -> Printf.fprintf out "<li>%a</li>\n" pp_ident id) elts;
      Printf.fprintf out "</ul>\n"
    end
    
let pp_exported_ident (type a) (out:out_channel) (id,src:u_ident*exp_source) : unit =
  Printf.fprintf out "%a%a" pp_ident id pp_exp_source src

let pp_exported_operation (type a) (out:out_channel) (id,src:u_ident*exp_source_op) : unit =
  Printf.fprintf out "%a%a" pp_ident id pp_exp_source_op src

let pp_visible_set (out:out_channel) (set,src:loc Component.set*vis_source) : unit =
  match set with
  | Component.Abstract_Set id ->
    Printf.fprintf out "%a%a" pp_ident id pp_vis_source src
  | Component.Concrete_Set (id,elts) ->
    begin
      Printf.fprintf out "%a%a\n" pp_ident id pp_vis_source src;
      Printf.fprintf out "<ul>\n";
      List.iter (fun id -> Printf.fprintf out "<li>%a</li>\n" pp_ident id) elts;
      Printf.fprintf out "</ul>\n"
    end
    
let pp_visible_ident (type a) (out:out_channel) (id,src:u_ident*vis_source) : unit =
  Printf.fprintf out "%a%a" pp_ident id pp_vis_source src

let pp_visible_operation (type a) (out:out_channel) (id,src:u_ident*vis_source_op) : unit =
  Printf.fprintf out "%a%a" pp_ident id pp_vis_source_op src

let print_list (type a) (out:out_channel) (title:string) (pp:out_channel -> a -> unit) (lst: a list) : unit =
  match lst with
  | [] -> ()
  | _ ->
    begin
      Printf.fprintf out "%s" title;
      Printf.fprintf out "<ul>\n";
      List.iter (fun x -> Printf.fprintf out "<li>%a</li>\n" pp x) lst;
      Printf.fprintf out "</ul>\n"
    end

let generate_machine_page (root_dir:string) (mch:string) (view:View.component_view) : unit =
  let out = open_out (root_dir ^ "/machines/" ^ mch ^ ".html") in
  (*Header*)
  generate_html_header out;
  (*Title*)
  Printf.fprintf out "<h1>[%a] %s</h1>\n" pp_kind view.component_kind mch;
  (*Parameters*)
  print_list out "<h2>Parameters</h2>\n" pp_ident view.parameters;
  (*Refines*)
  ( match view.component_kind with
    | Machine -> ()
    | Refinement ref | Implementation ref ->
      Printf.fprintf out "<h2>Refines</h2> <ul><li><a href=\"%s.html\">%s</a></li></ul>\n" (snd ref) (snd ref) );
  (*Dependencies*)
  print_list out "<h2>Dependencies</h2>\n" pp_dep view.dependencies;
  (*Refined by*)
  print_list out "<h2>Refined By</h2>\n" pp_refined_by view.refined_by;
  (*Required by*)
  print_list out "<h2>Required By</h2>\n" pp_dep view.required_by;
  (*Exported Identifiers*)
  Printf.fprintf out "<h2>Exported Items</h2>\n";
  print_list out "<h3>Sets</h3>\n" pp_exported_set view.exported_sets;
  print_list out "<h3>Concrete Constants</h3>\n" pp_exported_ident view.exported_concrete_constants;
  print_list out "<h3>Abstract Constants</h3>\n" pp_exported_ident view.exported_abstract_constants;
  print_list out "<h3>Concrete Variables</h3>\n" pp_exported_ident view.exported_concrete_variables;
  print_list out "<h3>Abstract Variables</h3>\n" pp_exported_ident view.exported_abstract_variables;
  print_list out "<h3>Operations</h3>\n" pp_exported_operation view.exported_operations;
  (*Visible Identifiers*)
  Printf.fprintf out "<h2>Visible Items</h2>\n";
  print_list out "<h3>Sets</h3>\n" pp_visible_set view.visible_sets;
  print_list out "<h3>Concrete Constants</h3>\n" pp_visible_ident view.visible_concrete_constants;
  print_list out "<h3>Abstract Constants</h3>\n" pp_visible_ident view.visible_abstract_constants;
  print_list out "<h3>Concrete Variables</h3>\n" pp_visible_ident view.visible_concrete_variables;
  print_list out "<h3>Abstract Variables</h3>\n" pp_visible_ident view.visible_abstract_variables;
  print_list out "<h3>Operations</h3>\n" pp_visible_operation view.visible_operations;
  (*Footer*)
  generate_html_footer out
