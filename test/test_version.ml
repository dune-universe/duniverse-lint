let pp_msg ppf = function `Msg s -> Format.fprintf ppf "%s" s

let equal_msg a b =
  match (a, b) with
  (* We don't really care about the wording of the error *)
  | `Msg _, `Msg _ -> true
  | _, _ -> false

let message = Alcotest.testable pp_msg equal_msg
let result = Alcotest.result Alcotest.unit message
let verify = Duniverse_lint.Lint.Private.check_version

let test_dune_suffix () =
  let valid = Ok () in
  let invalid = Error (`Msg "_") in
  Alcotest.check result "Regular valid" valid (verify "1.0+dune");
  Alcotest.check result "Revised valid" valid (verify "1.0+dune2");
  Alcotest.check result "Non-alphanumeric revised" invalid (verify "1.0+duneN");
  Alcotest.check result "Missing suffix" invalid (verify "1.0");
  Alcotest.check result "Not a suffix" invalid (verify "+dune1.0");
  Alcotest.check result "Whitespace suffix" invalid (verify "1.0+dune ");
  Alcotest.check result "Multiple suffixes" valid (verify "1.0+mirage+dune");
  Alcotest.check result "Multiple identical suffixes" valid
    (verify "1.0+dune+dune")

let test_case = Alcotest.test_case
let tests = ("version", [ test_case "Dune suffix" `Quick test_dune_suffix ])
