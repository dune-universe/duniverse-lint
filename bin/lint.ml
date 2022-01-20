open Base
module L = Duniverse_lint
open L.O

let check () =
  let* () = L.Lint.check_dune_project "dune-project" in
  let entries = L.Entries.opam_files "." in
  let checks = List.map entries ~f:L.Lint.opam_uses_dune in
  let* () = Result.all_unit checks in
  Ok ()

let main () =
  match check () with
  | Ok () -> 0
  | Error (`Msg msg) ->
      Stdio.print_endline msg;
      1

let () =
  let open Cmdliner in
  let duniverse_lint = Term.(const main $ const ()) in
  Term.exit_status @@ Term.eval (duniverse_lint, Term.info "duniverse-lint")
