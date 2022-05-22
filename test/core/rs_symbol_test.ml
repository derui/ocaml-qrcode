module S = Ocaml_qrcode_core.Rs_symbol

let symbol v = Stdint.Uint8.to_int @@ S.of_symbol v

let get_symbol_test () =
  Alcotest.(check' int) ~msg:"at 9" ~expected:0b00111010 ~actual:(symbol 9);
  Alcotest.(check' int) ~msg:"at 10" ~expected:0b01110100 ~actual:(symbol 10);
  Alcotest.(check' int) ~msg:"at 254" ~expected:0b10001110 ~actual:(symbol 254)

let tests = [ Alcotest.test_case "get symbol" `Quick get_symbol_test ]
