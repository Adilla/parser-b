let verbose = ref false

let set_verbose b = verbose := b

let write x =
  if !verbose then Printf.fprintf stdout x
  else Printf.ifprintf stdout x
