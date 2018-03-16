open Ocamlbuild_plugin
let _ = flag ["profile"; "link"] (S [A "-ccopt"; A "-no-pie"])
