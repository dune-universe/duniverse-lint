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
let verify = Duniverse_lint.Lint.Private.check_version

let test_dune_suffix () =
  let valid = `Finished (Ok ()) in
  let invalid = `Finished (Error (`Msg "_")) in
  Alcotest.check finished "Regular valid" valid (verify "1.0+dune");
  Alcotest.check finished "Revised valid" valid (verify "1.0+dune2");
  Alcotest.check finished "Non-alphanumeric revised" invalid
    (verify "1.0+duneN");
  Alcotest.check finished "Missing suffix" invalid (verify "1.0");
  Alcotest.check finished "Not a suffix" invalid (verify "+dune1.0");
  Alcotest.check finished "Whitespace suffix" invalid (verify "1.0+dune ");
  Alcotest.check finished "Multiple suffixes" valid (verify "1.0+mirage+dune");
  Alcotest.check finished "Multiple identical suffixes" valid
    (verify "1.0+dune+dune")

let test_case = Alcotest.test_case
let tests = ("version", [ test_case "Dune suffix" `Quick test_dune_suffix ])
