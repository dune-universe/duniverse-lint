let pp_msg ppf = function `Msg s -> Format.fprintf ppf "%s" s

let equal_msg a b =
  match (a, b) with
  (* We don't really care about the wording of the error *)
  | `Msg _, `Msg _ -> true
  | _, _ -> false

let equal_finished testable a b =
  match (a, b) with
  | `Finished a, `Finished b ->
      let equal = Alcotest.equal testable in
      equal a b

let pp_finished testable ppf = function
  | `Finished t ->
      let pp = Alcotest.pp testable in
      Format.fprintf ppf "`Testable %a" pp t

let message = Alcotest.testable pp_msg equal_msg
let result = Alcotest.result Alcotest.unit message
let finished = Alcotest.testable (pp_finished result) (equal_finished result)

let mk_test_dune_suffix expected name version_string () =
  Alcotest.check finished name expected
    (Duniverse_lint.Lint.Private.check_version version_string)

let mk_test expected name version_string =
  Alcotest.test_case name `Quick
    (mk_test_dune_suffix expected name version_string)

let valid = `Finished (Ok ())
let invalid = `Finished (Error (`Msg "_"))

let test_cases =
  [
    mk_test valid "Regular valid" "1.0+dune";
    mk_test valid "Revised valid" "1.0+dune2";
    mk_test valid "Multiple suffixes" "1.0+mirage+dune";
    mk_test valid "Multiple identical suffixes" "1.0+dune+dune";
    mk_test invalid "Non-alphanumeric revised" "1.0+duneN";
    mk_test invalid "Missing suffix" "1.0";
    mk_test invalid "Not a suffix" "+dune1.0";
    mk_test invalid "Whitespace suffix" "1.0+dune ";
  ]

let tests = ("version", test_cases)
