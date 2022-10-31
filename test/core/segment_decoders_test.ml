module B = Ocaml_qrcode_core.Bit_stream
module M = Ocaml_qrcode_core.Metadata
module V = Ocaml_qrcode_core.Version
module Mode = Ocaml_qrcode_core.Mode
module E = Ocaml_qrcode_core.Error_correction_level
module S = Ocaml_qrcode_core.Segment
module CI = Ocaml_qrcode_core.Count_indicator
open Support

module Number = struct
  module Encoder = Ocaml_qrcode_core.Segment_encoders.Number
  module Decoder = Ocaml_qrcode_core.Segment_decoders.Number

  let decode_test () =
    let metadata = M.make ~version:V.V_1 ~mode:Mode.Number ~error_correction_level:E.Low in
    let generator = data_to_generator ("01234567" |> String.to_seq |> List.of_seq) in

    let encoded = Encoder.encode ~metadata ~generator |> Result.get_ok in
    let decoded = Decoder.decode encoded in
    Alcotest.(check' @@ list char) ~msg:"mode" ~expected:[ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7' ] ~actual:decoded
end

module Alphabet = struct
  module Encoder = Ocaml_qrcode_core.Segment_encoders.Alphabet
  module Decoder = Ocaml_qrcode_core.Segment_decoders.Alphabet

  let decode_test () =
    let metadata = M.make ~version:V.V_1 ~mode:Mode.Alphabet ~error_correction_level:E.High in
    let generator = data_to_generator ("AC-42" |> String.to_seq |> List.of_seq) in

    let encoded = Encoder.encode ~metadata ~generator |> Result.get_ok in
    let decoded = Decoder.decode encoded in
    Alcotest.(check' @@ list char) ~msg:"bits" ~expected:[ 'A'; 'C'; '-'; '4'; '2' ] ~actual:decoded
end

module Byte = struct
  module Encoder = Ocaml_qrcode_core.Segment_encoders.Byte
  module Decoder = Ocaml_qrcode_core.Segment_decoders.Byte

  let decode_test () =
    let metadata = M.make ~version:V.V_1 ~mode:Mode.Byte ~error_correction_level:E.High in
    let generator = data_to_generator ("abcA3?|" |> String.to_seq |> List.of_seq) in

    let encoded = Encoder.encode ~metadata ~generator |> Result.get_ok in
    let actual = Decoder.decode encoded in
    Alcotest.(check' @@ list char) ~msg:"byte" ~expected:[ 'a'; 'b'; 'c'; 'A'; '3'; '?'; '|' ] ~actual
end

let tests =
  [
    Alcotest.test_case "can decode valid number data" `Quick Number.decode_test;
    Alcotest.test_case "can decode valid alphabet data" `Quick Alphabet.decode_test;
    Alcotest.test_case "can decode valid byte data" `Quick Byte.decode_test;
  ]
