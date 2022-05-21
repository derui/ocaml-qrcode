module B = Ocaml_qrcode_core.Bit_stream

let loop count f =
  let rec loop' index =
    if index >= count then ()
    else (
      f index;
      loop' (succ index))
  in
  loop' 0

let result_test = Alcotest.testable B.pp_bit_result ( = )

let create_test () =
  let stream = B.create () in
  let ret = B.next stream in
  Alcotest.(check result_test) "end of stream" B.Eos ret

let put_zero_test () =
  let stream = B.create () in
  let stream = B.put ~bit:`Zero stream in
  let ret = B.next stream in
  Alcotest.(check result_test) "end of stream" B.(Continue `Zero) ret

let put_one_test () =
  let stream = B.create () in
  let stream = B.put ~bit:`One stream in
  let ret = B.next stream in
  Alcotest.(check result_test) "end of stream" B.(Continue `One) ret

let put_multi_value_test () =
  let stream = B.create () in
  let stream = B.put ~bit:`One stream |> B.put ~bit:`Zero |> B.put ~bit:`One in
  let ret1 = B.next stream in
  let ret2 = B.next stream in
  let ret3 = B.next stream in
  let ret4 = B.next stream in
  Alcotest.(check result_test) "end of stream" B.(Continue `One) ret1;
  Alcotest.(check result_test) "end of stream" B.(Continue `Zero) ret2;
  Alcotest.(check result_test) "end of stream" B.(Continue `One) ret3;
  Alcotest.(check result_test) "end of stream" B.Eos ret4

let put_1k_value_test () =
  let stream = ref @@ B.create () in
  loop 1000 (fun index ->
      let bit = match index mod 2 with 1 -> `One | _ -> `Zero in
      stream := B.put ~bit !stream);

  loop 1000 (fun index ->
      let value = B.next !stream in
      let message = Printf.sprintf "count %d" index in
      let data = match index mod 2 with 1 -> `One | _ -> `Zero in
      Alcotest.(check result_test) message B.(Continue data) value)

let puts_1k_value_test () =
  let stream = B.create () in
  let list = List.init 1000 (fun index -> match index mod 2 with 1 -> `One | _ -> `Zero) in
  let stream = B.puts ~data:list stream in

  loop 1000 (fun index ->
      let value = B.next stream in
      let message = Printf.sprintf "count %d" index in
      let data = match index mod 2 with 1 -> `One | _ -> `Zero in
      Alcotest.(check result_test) message B.(Continue data) value)

let clone_test () =
  let stream = B.create () in
  let stream = B.put ~bit:`One stream in
  let cloned = B.clone stream in
  let actual = B.next stream and expected = B.next cloned in
  Alcotest.(check' result_test) ~msg:"same bit" ~actual ~expected

let put_int32_test () =
  let stream = B.create () in
  let data = (1 lsl 3) lor (1 lsl 1) lor 1 |> Int32.of_int in
  let stream = B.put_int32 ~data ~bits:4 stream in
  let ret1 = B.next stream in
  let ret2 = B.next stream in
  let ret3 = B.next stream in
  let ret4 = B.next stream in
  Alcotest.(check' result_test) ~msg:"1st bit" ~expected:B.(Continue `One) ~actual:ret1;
  Alcotest.(check' result_test) ~msg:"2nd bit" ~expected:B.(Continue `Zero) ~actual:ret2;
  Alcotest.(check' result_test) ~msg:"3rd bit" ~expected:B.(Continue `One) ~actual:ret3;
  Alcotest.(check' result_test) ~msg:"4th bit" ~expected:B.(Continue `One) ~actual:ret4

let put_zero_leading_int32_test () =
  let stream = B.create () in
  let data = (1 lsl 3) lor (1 lsl 1) lor 1 |> Int32.of_int in
  let stream = B.put_int32 ~data ~bits:8 stream in
  let ret1 = B.next stream in
  let ret2 = B.next stream in
  let ret3 = B.next stream in
  let ret4 = B.next stream in
  let ret5 = B.next stream in
  let ret6 = B.next stream in
  let ret7 = B.next stream in
  let ret8 = B.next stream in
  Alcotest.(check' result_test) ~msg:"position 1" ~expected:B.(Continue `Zero) ~actual:ret1;
  Alcotest.(check' result_test) ~msg:"position 2" ~expected:B.(Continue `Zero) ~actual:ret2;
  Alcotest.(check' result_test) ~msg:"position 3" ~expected:B.(Continue `Zero) ~actual:ret3;
  Alcotest.(check' result_test) ~msg:"position 4" ~expected:B.(Continue `Zero) ~actual:ret4;
  Alcotest.(check' result_test) ~msg:"position 5" ~expected:B.(Continue `One) ~actual:ret5;
  Alcotest.(check' result_test) ~msg:"position 6" ~expected:B.(Continue `Zero) ~actual:ret6;
  Alcotest.(check' result_test) ~msg:"position 7" ~expected:B.(Continue `One) ~actual:ret7;
  Alcotest.(check' result_test) ~msg:"position 8" ~expected:B.(Continue `One) ~actual:ret8

let concat_test () =
  let first = B.create () in
  let last = B.create () in
  let first = B.puts ~data:[ `One; `Zero ] first in
  let last = B.puts ~data:[ `Zero; `One ] last in
  let stream = B.concat ~first ~last in
  let ret1 = B.next stream in
  let ret2 = B.next stream in
  let ret3 = B.next stream in
  let ret4 = B.next stream in
  Alcotest.(check' result_test) ~msg:"1st bit" ~expected:B.(Continue `One) ~actual:ret1;
  Alcotest.(check' result_test) ~msg:"2nd bit" ~expected:B.(Continue `Zero) ~actual:ret2;
  Alcotest.(check' result_test) ~msg:"3rd bit" ~expected:B.(Continue `Zero) ~actual:ret3;
  Alcotest.(check' result_test) ~msg:"4th bit" ~expected:B.(Continue `One) ~actual:ret4

let tests =
  [
    Alcotest.test_case "can create new stream" `Quick create_test;
    Alcotest.test_case "can put zero into stream" `Quick put_zero_test;
    Alcotest.test_case "can put one into stream" `Quick put_one_test;
    Alcotest.test_case "can put multi value into" `Quick put_multi_value_test;
    Alcotest.test_case "can put 1,000 values" `Quick put_1k_value_test;
    Alcotest.test_case "can put 1,000 values one time" `Quick puts_1k_value_test;
    Alcotest.test_case "can clone stream without affection" `Quick clone_test;
    Alcotest.test_case "can put int32 with bits" `Quick put_int32_test;
    Alcotest.test_case "can put int32 that leading zero bits" `Quick put_zero_leading_int32_test;
    Alcotest.test_case "can concatenate two bit streams" `Quick concat_test;
  ]
