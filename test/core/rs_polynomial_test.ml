module S = Ocaml_qrcode_core.Rs_symbol
module P = Ocaml_qrcode_core.Rs_polynomial

let s v = S.of_symbol v

let invalid_polynomial_test () =
  Alcotest.(check' @@ option @@ of_pp Fmt.nop) ~msg:"not defined" ~expected:None ~actual:(P.to_validated 1);
  Alcotest.(check' @@ option @@ of_pp Fmt.nop) ~msg:"not defined" ~expected:None ~actual:(P.to_validated 69)

let valid_2_polynomial_test () =
  let actual = P.to_validated 2 |> Option.map P.from_validated |> Option.get in
  let expected = [| s 0; s 25; s 1 |] in
  Alcotest.(check' @@ array @@ of_pp S.pp) ~msg:"defined" ~expected ~actual

let valid_5_polynomial_test () =
  let actual = P.to_validated 5 |> Option.map P.from_validated |> Option.get in
  let expected = [| s 0; s 113; s 164; s 166; s 119; s 10 |] in
  Alcotest.(check' @@ array @@ of_pp S.pp) ~msg:"defined" ~expected ~actual

let tests =
  [
    Alcotest.test_case "invalid polynomial" `Quick invalid_polynomial_test;
    Alcotest.test_case "valid polynomial for 2" `Quick valid_2_polynomial_test;
    Alcotest.test_case "valid polynomial for 5" `Quick valid_5_polynomial_test;
  ]
