module L = Duniverse_lint

let main () =
  let _success = L.Lint.check_dune_project "dune-project" in
  Stdio.print_endline "Hooray"

let () = main ()
