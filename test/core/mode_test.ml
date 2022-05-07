module B = Ocaml_qrcode_core.Bit_stream
module M = Ocaml_qrcode_core.Mode

let result_test = Alcotest.testable B.pp_bit_result ( = )

let number_to_bits_test () =
  let stream = B.create () in
  let stream = M.to_bits ~stream M.Number in
  let first = B.next stream in
  let second = B.next stream in
  let third = B.next stream in
  let fourth = B.next stream in

  Alcotest.(check result_test) "first" B.(Continue `Zero) first;
  Alcotest.(check result_test) "second" B.(Continue `Zero) second;
  Alcotest.(check result_test) "third" B.(Continue `Zero) third;
  Alcotest.(check result_test) "fourth" B.(Continue `One) fourth

let alphabet_to_bits_test () =
  let stream = B.create () in
  let stream = M.to_bits ~stream M.Alphabet in
  let first = B.next stream in
  let second = B.next stream in
  let third = B.next stream in
  let fourth = B.next stream in

  Alcotest.(check result_test) "first" B.(Continue `Zero) first;
  Alcotest.(check result_test) "second" B.(Continue `Zero) second;
  Alcotest.(check result_test) "third" B.(Continue `One) third;
  Alcotest.(check result_test) "fourth" B.(Continue `Zero) fourth

let byte_to_bits_test () =
  let stream = B.create () in
  let stream = M.to_bits ~stream M.Byte in
  let first = B.next stream in
  let second = B.next stream in
  let third = B.next stream in
  let fourth = B.next stream in

  Alcotest.(check result_test) "first" B.(Continue `Zero) first;
  Alcotest.(check result_test) "second" B.(Continue `One) second;
  Alcotest.(check result_test) "third" B.(Continue `Zero) third;
  Alcotest.(check result_test) "fourth" B.(Continue `Zero) fourth

let tests =
  [
    Alcotest.test_case "can convert bits from number" `Quick number_to_bits_test;
    Alcotest.test_case "can convert bits from alphabet" `Quick alphabet_to_bits_test;
    Alcotest.test_case "can convert bits from byte" `Quick byte_to_bits_test;
  ]
