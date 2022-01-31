open Base
module L = Duniverse_lint
open L.O

let check () =
  let checks = L.Lint.dune_project "dune-project" in
  let entries = L.Entries.opam_files "." in
  let more = List.map entries ~f:L.Lint.opam_uses_dune in
  let checks = checks :: more in
  let all = L.Lint.all ~label:"Linting project" checks in
  let results = L.Lint.eval all in
  let* () = Result.all_unit results in
  Ok ()

let main () = match check () with Ok () -> 0 | Error _ -> 1

let () =
  let open Cmdliner in
  let duniverse_lint = Term.(const main $ const ()) in
  Term.exit_status @@ Term.eval (duniverse_lint, Term.info "duniverse-lint")
