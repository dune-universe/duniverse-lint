(lang dune 2.9)
(name duniverse-lint)

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
  (ocaml (>= 4.13.0))
  (base (>= v0.14))
  (cmdliner (>= 1.0.4))
  (opam-file-format (>= 2.1.2))
  (opam-format (>= 2.1.0))
  (sexplib (>= v0.14))
  (stdio (>= v0.14))))
