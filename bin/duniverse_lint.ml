module L = Duniverse_lint
open L.O

let check () =
  let* () = L.Lint.check_dune_project "dune-project" in
  Ok ()

let main () =
  match check () with
  | Ok () -> ()
  | Error (`Msg msg) -> Stdio.print_endline msg

let () = main ()
