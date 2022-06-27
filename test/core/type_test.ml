module B = Ocaml_qrcode_core.Type.Bit
open Support

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

let gf256_test () =
  let module G = Ocaml_qrcode_core.Type.GF256 in
  let uint8 = Stdint.Uint8.of_int in
  Alcotest.(check' uint8_testable) ~msg:"1 + 7" ~expected:(uint8 0b10000010) ~actual:G.(uint8 1 + uint8 7);
  Alcotest.(check' uint8_testable) ~msg:"1 * 7 = 8" ~expected:(G.of_uint8 @@ uint8 8) ~actual:G.(uint8 1 * uint8 7);
  Alcotest.(check' uint8_testable) ~msg:"8 * 25 = 33" ~expected:(G.of_uint8 @@ uint8 33) ~actual:G.(uint8 8 * uint8 25);
  Alcotest.(check' uint8_testable)
    ~msg:"254 * 50 = 304 - 255 = 49"
    ~expected:(G.of_uint8 @@ uint8 49)
    ~actual:G.(uint8 50 * uint8 254);
  ()

let tests = [ Alcotest.test_case "modulo2" `Quick bit_test; Alcotest.test_case "modulo256" `Quick gf256_test ]
