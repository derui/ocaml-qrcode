module B = Ocaml_qrcode_core.Bit_stream
module M = Ocaml_qrcode_core.Metadata
module V = Ocaml_qrcode_core.Version
module Mode = Ocaml_qrcode_core.Mode
module E = Ocaml_qrcode_core.Error_correction
module S = Ocaml_qrcode_core.Segment
module CI = Ocaml_qrcode_core.Count_indicator

let data_to_generator data =
  let data = ref data in
  fun () ->
    match !data with
    | [] -> None
    | v :: rest ->
        data := rest;
        Some v

let to_bit_string list =
  let rec loop accum = function
    | [] -> List.rev accum |> List.to_seq |> String.of_seq
    | v :: rest ->
        let bit = match v with `One -> '1' | `Zero -> '0' in
        loop (bit :: accum) rest
  in
  loop [] list

let error_testable = Alcotest.of_pp S.Encoding_error.pp

let mode_testable = Alcotest.result (Alcotest.of_pp Mode.pp) error_testable

let count_indicator_testable = Alcotest.result (Alcotest.of_pp Ocaml_qrcode_core.Count_indicator.pp) error_testable

module Number = struct
  module Encoder = Ocaml_qrcode_core.Segment_encoders.Number

  let encode_test () =
    let metadata = M.make ~version:V.V_1 ~mode:Mode.Number ~error_correction_level:E.Low in
    let generator = data_to_generator ("01234567" |> String.to_seq |> List.of_seq) in

    let encoded = Encoder.encode ~metadata ~generator in
    let mode = Result.map (fun v -> v.S.mode) encoded in
    let indicator = Result.map (fun v -> v.S.count_indicator) encoded in
    let bit_string = Result.get_ok encoded |> fun v -> B.to_list v.data |> to_bit_string in
    let indicator_expected = CI.make ~mode:Mode.Number ~version:V.V_1 |> CI.set_count ~count:8 in
    Alcotest.(check' mode_testable) ~msg:"mode" ~expected:(Ok Mode.Number) ~actual:mode;
    Alcotest.(check' count_indicator_testable) ~msg:"indicator" ~expected:(Ok indicator_expected) ~actual:indicator;
    Alcotest.(check' string) ~msg:"bits" ~expected:"000000110001010110011000011" ~actual:bit_string

  let invalid_data_test () =
    let metadata = M.make ~version:V.V_1 ~mode:Mode.Number ~error_correction_level:E.Low in
    let generator = data_to_generator ("09a34" |> String.to_seq |> List.of_seq) in

    let encoded = Encoder.encode ~metadata ~generator in
    Alcotest.(check' @@ result (of_pp Fmt.nop) error_testable)
      ~msg:"error"
      ~expected:(Error (S.Encoding_error.Invalid_data "Can not use character 'a' in number mode"))
      ~actual:encoded

  let size_overflow_test () =
    let metadata = M.make ~version:V.V_1 ~mode:Mode.Number ~error_correction_level:E.High in
    let generator = data_to_generator ("123456789012345678" |> String.to_seq |> List.of_seq) in

    let encoded = Encoder.encode ~metadata ~generator in
    Alcotest.(check' @@ result (of_pp Fmt.nop) error_testable)
      ~msg:"error"
      ~expected:(Error (S.Encoding_error.Data_size_overflow ("Can not accept size of data greater than", 17)))
      ~actual:encoded
end

module Alphabet = struct
  module Encoder = Ocaml_qrcode_core.Segment_encoders.Alphabet

  let encode_test () =
    let metadata = M.make ~version:V.V_1 ~mode:Mode.Alphabet ~error_correction_level:E.High in
    let generator = data_to_generator ("AC-42" |> String.to_seq |> List.of_seq) in

    let encoded = Encoder.encode ~metadata ~generator in
    let mode = Result.map (fun v -> v.S.mode) encoded in
    let indicator = Result.map (fun v -> v.S.count_indicator) encoded in
    let bit_string = Result.get_ok encoded |> fun v -> B.to_list v.data |> to_bit_string in
    let indicator_expected = CI.make ~mode:Mode.Alphabet ~version:V.V_1 |> CI.set_count ~count:5 in
    Alcotest.(check' mode_testable) ~msg:"mode" ~expected:(Ok Mode.Alphabet) ~actual:mode;
    Alcotest.(check' count_indicator_testable) ~msg:"indicator" ~expected:(Ok indicator_expected) ~actual:indicator;
    Alcotest.(check' string) ~msg:"bits" ~expected:"0011100111011100111001000010" ~actual:bit_string

  let invalid_data_test () =
    let metadata = M.make ~version:V.V_1 ~mode:Mode.Alphabet ~error_correction_level:E.Low in
    let generator = data_to_generator ("a!b" |> String.to_seq |> List.of_seq) in

    let encoded = Encoder.encode ~metadata ~generator in
    Alcotest.(check' @@ result (of_pp Fmt.nop) error_testable)
      ~msg:"error"
      ~expected:(Error (S.Encoding_error.Invalid_data "Can not use character 'a' in alphabet mode"))
      ~actual:encoded

  let size_overflow_test () =
    let metadata = M.make ~version:V.V_1 ~mode:Mode.Alphabet ~error_correction_level:E.High in
    let generator = data_to_generator ("12345678901" |> String.to_seq |> List.of_seq) in

    let encoded = Encoder.encode ~metadata ~generator in
    Alcotest.(check' @@ result (of_pp Fmt.nop) error_testable)
      ~msg:"error"
      ~expected:(Error (S.Encoding_error.Data_size_overflow ("Can not accept size of data greater than", 10)))
      ~actual:encoded
end

module Byte = struct
  module Encoder = Ocaml_qrcode_core.Segment_encoders.Byte

  let encode_test () =
    let metadata = M.make ~version:V.V_1 ~mode:Mode.Byte ~error_correction_level:E.High in
    let generator = data_to_generator ("abcA3|" |> String.to_seq |> List.of_seq) in

    let encoded = Encoder.encode ~metadata ~generator in
    let mode = Result.map (fun v -> v.S.mode) encoded in
    let indicator = Result.map (fun v -> v.S.count_indicator) encoded in
    let bit_string = Result.get_ok encoded |> fun v -> B.to_list v.data |> to_bit_string in
    let indicator_expected = CI.make ~mode:Mode.Byte ~version:V.V_1 |> CI.set_count ~count:6 in
    Alcotest.(check' mode_testable) ~msg:"mode" ~expected:(Ok Mode.Byte) ~actual:mode;
    Alcotest.(check' count_indicator_testable) ~msg:"indicator" ~expected:(Ok indicator_expected) ~actual:indicator;
    Alcotest.(check' string) ~msg:"bits" ~expected:"011000010110001001100011010000010011001101111100" ~actual:bit_string

  let size_overflow_test () =
    let metadata = M.make ~version:V.V_1 ~mode:Mode.Byte ~error_correction_level:E.High in
    let generator = data_to_generator ("12345678" |> String.to_seq |> List.of_seq) in

    let encoded = Encoder.encode ~metadata ~generator in
    Alcotest.(check' @@ result (of_pp Fmt.nop) error_testable)
      ~msg:"error"
      ~expected:(Error (S.Encoding_error.Data_size_overflow ("Can not accept size of data greater than", 7)))
      ~actual:encoded
end

let tests =
  [
    Alcotest.test_case "can encode valid number data" `Quick Number.encode_test;
    Alcotest.test_case "can not encode if generator returns invalid data" `Quick Number.invalid_data_test;
    Alcotest.test_case "can not encode if generator large size" `Quick Number.size_overflow_test;
    Alcotest.test_case "can encode valid alphabet data" `Quick Alphabet.encode_test;
    Alcotest.test_case "can not encode if generator returns invalid data" `Quick Alphabet.invalid_data_test;
    Alcotest.test_case "can not encode if generator large size" `Quick Alphabet.size_overflow_test;
    Alcotest.test_case "can encode byte data" `Quick Byte.encode_test;
    Alcotest.test_case "can not encode if generator large size" `Quick Byte.size_overflow_test;
  ]
