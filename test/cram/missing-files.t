The linter should complain about missing files but not crash.

  $ duniverse-lint
  `dune-project` file missing
  No opam files found

Creating these files should help:

  $ echo '(name cram)(version 1.0+dune)' > dune-project
  $ echo 'opam-version: "2.0"' > cram.opam
  $ duniverse-lint | grep file
  [1]

Now the linter should not complain about files anymore
