open Base
module L = Duniverse_lint
open L.O

let check () =
  let entries = L.Entries.opam_files "." in
  let project = L.Lint.check_dune_project "dune-project" in
  let opam_exists = L.Lint.opam_files_exist entries in
  let opam_files = List.map entries ~f:L.Lint.opam_uses_dune in
  let all_checks = project :: opam_exists :: opam_files in
  let* () = Result.combine_errors_unit all_checks in
  Ok ()

let main () =
  match check () with
  | Ok () -> ()
  | Error messages ->
      List.iter ~f:(function `Msg msg -> Stdio.print_endline msg) messages

let () =
  let open Cmdliner in
  let duniverse_lint = Term.(const main $ const ()) in
  Term.exit @@ Term.eval (duniverse_lint, Term.info "duniverse-lint")
