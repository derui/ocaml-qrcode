module B = Ocaml_qrcode_core.Bit_stream

let result_test = Alcotest.testable B.pp_bit_result ( = )

let create_test () =
  let stream = B.create () in
  let ret = B.next stream in
  Alcotest.(check result_test) "end of stream" B.Eos ret

let put_zero_test () =
  let stream = B.create () in
  let stream = B.put ~data:`Zero stream in
  let ret = B.next stream in
  Alcotest.(check result_test) "end of stream" B.(Continue `Zero) ret

let put_one_test () =
  let stream = B.create () in
  let stream = B.put ~data:`One stream in
  let ret = B.next stream in
  Alcotest.(check result_test) "end of stream" B.(Continue `One) ret

let tests =
  [
    Alcotest.test_case "can create new stream" `Quick create_test;
    Alcotest.test_case "can put zero into stream" `Quick put_zero_test;
    Alcotest.test_case "can put one into stream" `Quick put_one_test;
  ]
