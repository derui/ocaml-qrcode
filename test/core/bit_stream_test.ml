module B = Ocaml_qrcode_core.Bit_stream

let loop count f =
  let rec loop' index =
    if index >= count then ()
    else (
      f index;
      loop' (succ index))
  in
  loop' 0

let bit_string s =
  String.to_seq s
  |> Seq.filter (function ' ' -> false | _ -> true)
  |> Seq.map (function '1' -> `One | '0' -> `Zero | _ -> failwith "Invalid")
  |> List.of_seq

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

let count_test () =
  let stream = B.create () in
  let stream = B.put_int32 ~data:5l ~bits:4 stream in
  let stream = B.put_byte ~data:(Stdint.Uint8.of_int 15) stream in
  let stream = B.put ~bit:`One stream in
  let ret = B.count stream in
  Alcotest.(check int) "size of stream" 13 ret

let put_one_test () =
  let stream = B.create () in
  let stream = B.put ~bit:`One stream in
  let ret = B.next stream in
  Alcotest.(check result_test) "end of stream" B.(Continue `One) ret

let put_byte_test () =
  let module U = Stdint.Uint8 in
  let stream = B.create () in
  let stream = B.put_byte ~data:(U.of_int 0b1100) stream in
  let ret = B.to_byte_list stream in
  Alcotest.(check @@ list Support.uint8_testable) "byte" [ U.of_int 0b1100 ] ret

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
  let data = 0b01011 |> Int32.of_int in
  let stream = B.put_int32 ~data ~bits:5 stream in
  let ret1 = B.next stream in
  let ret2 = B.next stream in
  let ret3 = B.next stream in
  let ret4 = B.next stream in
  let ret5 = B.next stream in
  Alcotest.(check' result_test) ~msg:"1st bit" ~expected:B.(Continue `Zero) ~actual:ret1;
  Alcotest.(check' result_test) ~msg:"2nd bit" ~expected:B.(Continue `One) ~actual:ret2;
  Alcotest.(check' result_test) ~msg:"3rd bit" ~expected:B.(Continue `Zero) ~actual:ret3;
  Alcotest.(check' result_test) ~msg:"4th bit" ~expected:B.(Continue `One) ~actual:ret4;
  Alcotest.(check' result_test) ~msg:"5th bit" ~expected:B.(Continue `One) ~actual:ret5

let put_zero_leading_int32_test () =
  let stream = B.create () in
  let data = 0b1011 |> Int32.of_int in
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

let byte_list_test () =
  let stream = B.create () in
  let stream = B.puts ~data:(bit_string "0001 0000001000 0000001100 0101011001 1000011") stream in
  let ret = B.to_byte_list stream in
  Alcotest.(check' @@ list Support.uint8_testable)
    ~msg:"byte"
    ~expected:
      [
        Stdint.Uint8.of_int 0b00010000;
        Stdint.Uint8.of_int 0b00100000;
        Stdint.Uint8.of_int 0b00001100;
        Stdint.Uint8.of_int 0b01010110;
        Stdint.Uint8.of_int 0b01100001;
        Stdint.Uint8.of_int 0b1;
      ]
    ~actual:ret

let next_int32_test () =
  let stream = B.create () in
  let data = 0b1011l in
  let stream = B.put_int32 ~data ~bits:8 stream in
  let actual = B.next_int32 stream in
  Alcotest.(check' int32) ~msg:"position 1" ~expected:data ~actual

let next_int32_with_size_test () =
  let stream = B.create () in
  let data = 0b1011l in
  let stream = B.put_int32 ~data ~bits:4 stream in
  let actual = B.next_int32 ~size:2 stream in
  Alcotest.(check' int32) ~msg:"" ~expected:(0b10 |> Int32.of_int) ~actual

let tests =
  [
    Alcotest.test_case "can create new stream" `Quick create_test;
    Alcotest.test_case "can put zero into stream" `Quick put_zero_test;
    Alcotest.test_case "can byte into stream" `Quick put_byte_test;
    Alcotest.test_case "can get bit count in stream" `Quick count_test;
    Alcotest.test_case "can put one into stream" `Quick put_one_test;
    Alcotest.test_case "can put multi value into" `Quick put_multi_value_test;
    Alcotest.test_case "can put 1,000 values" `Quick put_1k_value_test;
    Alcotest.test_case "can put 1,000 values one time" `Quick puts_1k_value_test;
    Alcotest.test_case "can clone stream without affection" `Quick clone_test;
    Alcotest.test_case "can put int32 with bits" `Quick put_int32_test;
    Alcotest.test_case "can put int32 that leading zero bits" `Quick put_zero_leading_int32_test;
    Alcotest.test_case "can concatenate two bit streams" `Quick concat_test;
    Alcotest.test_case "can convert byte list" `Quick byte_list_test;
    Alcotest.test_case "can get next int32 from stream" `Quick next_int32_test;
    Alcotest.test_case "can get next int32 with size" `Quick next_int32_with_size_test;
  ]
