module B = Ocaml_qrcode_core.Bit_stream
module M = Ocaml_qrcode_core.Metadata
module V = Ocaml_qrcode_core.Version
module Mode = Ocaml_qrcode_core.Mode
module E = Ocaml_qrcode_core.Error_correction_level
module S = Ocaml_qrcode_core.Segment
module CI = Ocaml_qrcode_core.Count_indicator
module Encoder = Ocaml_qrcode_core.Segment_encoders.Number
module CW = Ocaml_qrcode_core.Code_word
module RS = Ocaml_qrcode_core.Reed_solomon_coding
open Support

let make_reed_solomon_code_test () =
  let module Encoder = Ocaml_qrcode_core.Segment_encoders.Byte in
  let metadata = M.make ~version:V.V_1 ~mode:Mode.Number ~error_correction_level:E.Low in
  let generator = data_to_generator ("12345678" |> String.to_seq |> List.of_seq) in
  let encoded = Encoder.encode ~metadata ~generator |> Result.get_ok in
  let code_word = CW.make ~segments:[ encoded ] ~metadata in
  let rs = RS.calculate_ec ~metadata code_word in
  let actual = rs.ec_blocks.(0) in
  let expected = [ 67; 175; 134; 187; 90; 13; 150 ] |> List.rev |> Array.of_list |> Array.map Stdint.Uint8.of_int in

  Alcotest.(check' code_word_testable) ~msg:"codeword" ~expected ~actual

let tests = [ Alcotest.test_case "make reed solomon code" `Quick make_reed_solomon_code_test ]
