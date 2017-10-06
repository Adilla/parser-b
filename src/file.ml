let paths = ref []

let add_path (p:string) : (unit,string) result =
  try
    if Sys.is_directory p then ( paths := p :: !paths; Ok () )
    else Error ("'"^p^"' is not a directory.")
  with Sys_error msg -> Error msg

let get_fullname (fn:string) : string option =
  let rec aux = function
    | [] -> None
    | p::lst ->
      let full = p ^ "/" ^ fn in
      if Sys.file_exists full then Some full
      else aux lst
  in
  aux ("." :: !paths)

let get_fullname_comp (cname:string) : string option =
  let rec aux = function
    | [] -> None
    | p::lst ->
      let full1 = p ^ "/" ^ cname ^ ".mch" in
      let full2 = p ^ "/" ^ cname ^ ".ref" in
      let full3 = p ^ "/" ^ cname ^ ".imp" in
      if Sys.file_exists full1 then Some full1
      else if Sys.file_exists full2 then Some full2
      else if Sys.file_exists full3 then Some full3
      else aux lst
  in
  aux ("." :: !paths)
