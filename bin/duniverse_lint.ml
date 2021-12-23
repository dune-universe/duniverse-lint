module L = Duniverse_lint
open L.O

let check () =
  let* () = L.Lint.check_dune_project "dune-project" in
  let* () = L.Lint.opam_uses_dune "duniverse-lint.opam" in
  Ok ()

let main () =
  let open Cmdliner in
  let duniverse_lint = Term.(const check $ const ()) in
  Term.exit @@ Term.eval (duniverse_lint, Term.info "duniverse-lint")

let () = main ()
