(lang dune 2.9)
(name duniverse-lint)

(cram enable)
(generate_opam_files true)

(source (github dune-universe/duniverse-lint))
(license ISC)
(authors "Marek Kubica")
(maintainers "Marek Kubica <marek@tarides.com>")

(package
 (name duniverse-lint)
 (synopsis "Checks duniverse ports for correctness")
 (description "`duniverse-lint` checks ports to `dune` for correctness")
 (depends
  (ocaml (>= 4.11.0))
  (alcotest (and :with-test (>= 1.5.0)))
  (base (>= v0.14))
  (cmdliner (>= 1.0.4))
  (opam-file-format (>= 2.1.2))
  (opam-format (>= 2.1.0))
  (re (>= 1.10.3))
  (sexplib (>= v0.14))
  (stdio (>= v0.14))))
