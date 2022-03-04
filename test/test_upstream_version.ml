let test name ~port ~upstream () =
  let upstreamed = Duniverse_lint.Lint.Private.upstream_version port in
  Alcotest.(check string) name upstreamed upstream

let mk_test name ~port ~upstream =
  Alcotest.test_case name `Quick (test name ~port ~upstream)

let test_cases =
  [
    mk_test "No changes" ~port:"1.0" ~upstream:"1.0";
    mk_test "Just +dune" ~port:"1.0+dune" ~upstream:"1.0";
    mk_test "Revised +dune" ~port:"1.0+dune2" ~upstream:"1.0";
    mk_test "Suffix twice" ~port:"1.0+dune1+dune2" ~upstream:"1.0+dune1";
    mk_test "Multiple suffixes" ~port:"1.0+mirage+dune" ~upstream:"1.0+mirage";
  ]

let tests = ("upstream version", test_cases)
