This test defines a project that should complete successfully:

  $ cat > dune-project <<EOF
  > (name cram)
  > (version 1.0+dune)
  > EOF
  $ cat > cram.opam <<EOF
  > opam-version: "2.0"
  > build: ["dune"]
  > EOF
  $ duniverse-lint

That's it, the linter doesn't complain about anything. Ship it!
