module B = Ocaml_qrcode_core.Bit_stream
module Number_encoder = Ocaml_qrcode_core.Segment_encoders.Number
module M = Ocaml_qrcode_core.Metadata
module V = Ocaml_qrcode_core.Version
module Mode = Ocaml_qrcode_core.Mode
module E = Ocaml_qrcode_core.Error_correction
module S = Ocaml_qrcode_core.Segment

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

let encode_test () =
  let metadata = M.make ~version:V.V_1 ~mode:Mode.Number ~error_correction_level:E.Low in
  let generator = data_to_generator ("01234567" |> String.to_seq |> List.of_seq) in

  let encoded = Number_encoder.encode ~metadata ~generator in
  let mode = Result.map (fun v -> v.S.mode) encoded in
  let indicator = Result.map (fun v -> v.S.count_indicator) encoded in
  let bit_string = Result.get_ok encoded |> fun v -> B.to_list v.data |> to_bit_string in
  let indicator_expected =
    let v = Ocaml_qrcode_core.Count_indicator.of_mode_with_version ~mode:Mode.Number ~version:V.V_1 in
    Ocaml_qrcode_core.Count_indicator.set_count v ~count:8
  in
  Alcotest.(check' mode_testable) ~msg:"mode" ~expected:(Ok Mode.Number) ~actual:mode;
  Alcotest.(check' count_indicator_testable) ~msg:"indicator" ~expected:(Ok indicator_expected) ~actual:indicator;
  Alcotest.(check' string) ~msg:"bits" ~expected:"000000110001010110011000011" ~actual:bit_string

let invalid_data_test () =
  let metadata = M.make ~version:V.V_1 ~mode:Mode.Number ~error_correction_level:E.Low in
  let generator = data_to_generator ("09a34" |> String.to_seq |> List.of_seq) in

  let encoded = Number_encoder.encode ~metadata ~generator in
  Alcotest.(check' @@ result (of_pp Fmt.nop) error_testable)
    ~msg:"error"
    ~expected:(Error (S.Encoding_error.Invalid_data "Can not use character 'a' in number mode"))
    ~actual:encoded

let size_overflow_test () =
  let metadata = M.make ~version:V.V_1 ~mode:Mode.Number ~error_correction_level:E.High in
  let generator = data_to_generator ("1234567890" |> String.to_seq |> List.of_seq) in

  let encoded = Number_encoder.encode ~metadata ~generator in
  Alcotest.(check' @@ result (of_pp Fmt.nop) error_testable)
    ~msg:"error"
    ~expected:(Error (S.Encoding_error.Data_size_overflow ("Can not accept size of data greater than", 9)))
    ~actual:encoded

let tests =
  [
    Alcotest.test_case "can encode valid number data" `Quick encode_test;
    Alcotest.test_case "can not encode if generator returns invalid data" `Quick invalid_data_test;
    Alcotest.test_case "can not encode if generator large size" `Quick size_overflow_test;
  ]
