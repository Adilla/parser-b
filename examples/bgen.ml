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

let pretty_print (x:Easy_format.t) : unit =
  Easy_format.Pretty.to_channel !out x

let args = [
  ("-o", Arg.String set_out, "Output file" );
  ("-k", Arg.String set_kind, "Kind of generation (expr|pred|subst|mch|ref|imp|comp) (default=comp)" )
]

let () =
  Arg.parse args (fun _ -> ()) ("Usage: "^ Sys.argv.(0) ^" [options]");
  Random.self_init ();
  let rd = Random.get_state () in
  let open Print in
  let x = match !kind with
    | Expr -> ef_expr (Generators.gen_expr rd)
    | Pred -> ef_pred (Generators.gen_pred rd)
    | Subst -> ef_subst (Generators.gen_subst rd)
    | Mch -> ef_machine (Generators.gen_machine rd)
    | Ref -> ef_refinement (Generators.gen_refinement rd)
    | Imp -> ef_implementation (Generators.gen_implementation rd)
    | Comp -> ef_component (Generators.gen_component rd)
  in
  pretty_print x
