module B = Ocaml_qrcode_core.Bit_stream
module M = Ocaml_qrcode_core.Metadata
module V = Ocaml_qrcode_core.Version
module Mode = Ocaml_qrcode_core.Mode
module E = Ocaml_qrcode_core.Error_correction_level
module S = Ocaml_qrcode_core.Segment
module CI = Ocaml_qrcode_core.Count_indicator
module Encoder = Ocaml_qrcode_core.Segment_encoders.Number
open Support

let output_to_bit_stream_test () =
  let metadata = M.make ~version:V.V_1 ~mode:Mode.Number ~error_correction_level:E.Low in
  let generator = data_to_generator ("01234567" |> String.to_seq |> List.of_seq) in
  let encoded = Encoder.encode ~metadata ~generator |> Result.get_ok in
  let stream = B.create () in
  let bit_string = S.output_to_bit_stream ~stream encoded |> B.to_list |> to_bit_string in
  Alcotest.(check' string) ~msg:"bits" ~expected:"00010000001000000000110001010110011000011" ~actual:bit_string

let tests = [ Alcotest.test_case "can output to bit stream" `Quick output_to_bit_stream_test ]
