module B = Ocaml_qrcode_core.Type.Bit

let test_bit = Alcotest.testable B.pp ( = )

let zero = `Zero

let one = `One

let bit_test () =
  Alcotest.(check test_bit) "0 + 0" zero B.(zero + zero);
  Alcotest.(check test_bit) "1 + 0" one B.(one + zero);
  Alcotest.(check test_bit) "0 + 1" one B.(zero + one);
  Alcotest.(check test_bit) "1 + 1" zero B.(one + one);
  Alcotest.(check test_bit) "0 * 0" zero B.(zero * zero);
  Alcotest.(check test_bit) "1 * 0" zero B.(one * zero);
  Alcotest.(check test_bit) "0 * 1" zero B.(zero * one);
  Alcotest.(check test_bit) "1 * 1" one B.(one * one)

let tests = [ Alcotest.test_case "modulo2" `Quick bit_test ]
