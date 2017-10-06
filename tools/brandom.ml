type kind_t = Expr | Pred | Subst | Mch | Ref | Imp | Comp

let kind = ref Comp
let out = ref stdout

let set_kind s =
  if String.equal s "expr" then kind := Expr
  else if String.equal s "pred" then kind := Pred
  else if String.equal s "subst" then kind := Subst
  else if String.equal s "mch" then kind := Mch
  else if String.equal s "ref" then kind := Ref
  else if String.equal s "imp" then kind := Imp
  else if String.equal s "comp" then kind := Comp

let set_out filename =
  out := open_out filename

let args = [
  ("-o", Arg.String set_out, "Output file" );
  ("-k", Arg.String set_kind, "Kind of generation (expr|pred|subst|mch|ref|imp|comp) (default=comp)" )
]

let () =
  Arg.parse args (fun _ -> ()) ("Usage: "^ Sys.argv.(0) ^" [options]");
  Random.self_init ();
  let rd = Random.get_state () in
  let open Print in
  match !kind with
    | Expr -> print_expression !out (Generators.gen_expr rd)
    | Pred -> print_predicate !out (Generators.gen_pred rd)
    | Subst -> print_substitution !out (Generators.gen_subst rd)
    | Mch -> print_component !out (Generators.gen_machine rd)
    | Ref -> print_component !out (Generators.gen_refinement rd)
    | Imp -> print_component !out (Generators.gen_implementation rd)
    | Comp -> print_component !out (Generators.gen_component rd)
