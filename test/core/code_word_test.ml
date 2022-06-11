module B = Ocaml_qrcode_core.Bit_stream
module M = Ocaml_qrcode_core.Metadata
module V = Ocaml_qrcode_core.Version
module Mode = Ocaml_qrcode_core.Mode
module E = Ocaml_qrcode_core.Error_correction_level
module S = Ocaml_qrcode_core.Segment
module CI = Ocaml_qrcode_core.Count_indicator
module Encoder = Ocaml_qrcode_core.Segment_encoders.Number
module CW = Ocaml_qrcode_core.Code_word
open Support

let fill_codewords count = List.init count (fun i -> if i mod 2 = 0 then 0b11101100 else 0b00010001)

let make_empty_data_test () =
  let metadata = M.make ~version:V.V_1 ~mode:Mode.Number ~error_correction_level:E.Low in
  let generator = data_to_generator ("" |> String.to_seq |> List.of_seq) in
  let encoded = Encoder.encode ~metadata ~generator |> Result.get_ok in
  let code_word = CW.make ~segments:[ encoded ] ~metadata in
  let expected = [ 0b00010000; 0b00000000; 0b00000000 ] @ fill_codewords 16 in
  let expected = List.map Stdint.Uint8.of_int expected |> Array.of_list in

  Alcotest.(check' code_word_testable) ~msg:"codeword" ~expected ~actual:code_word.CW.words

let make_full_data_test () =
  let module Encoder = Ocaml_qrcode_core.Segment_encoders.Byte in
  let metadata = M.make ~version:V.V_1 ~mode:Mode.Byte ~error_correction_level:E.High in
  let generator = data_to_generator ("1234567" |> String.to_seq |> List.of_seq) in
  let encoded = Encoder.encode ~metadata ~generator |> Result.get_ok in
  let code_word = CW.make ~segments:[ encoded ] ~metadata in
  let expected =
    [| 0b01000000; 0b01110011; 0b00010011; 0b00100011; 0b00110011; 0b01000011; 0b01010011; 0b01100011; 0b01110000 |]
    |> Array.map Stdint.Uint8.of_int
  in

  Alcotest.(check' code_word_testable) ~msg:"codeword" ~expected ~actual:code_word.CW.words

let tests =
  [
    Alcotest.test_case "can make codewords from empty data" `Quick make_empty_data_test;
    Alcotest.test_case "can make codewords from filled data" `Quick make_full_data_test;
  ]
