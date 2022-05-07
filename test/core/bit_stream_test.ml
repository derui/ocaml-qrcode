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

let tests =
  [
    Alcotest.test_case "can create new stream" `Quick create_test;
    Alcotest.test_case "can put zero into stream" `Quick put_zero_test;
    Alcotest.test_case "can put one into stream" `Quick put_one_test;
    Alcotest.test_case "can put multi value into" `Quick put_multi_value_test;
    Alcotest.test_case "can put 1,000 values" `Quick put_1k_value_test;
    Alcotest.test_case "can put 1,000 values one time" `Quick puts_1k_value_test;
  ]
