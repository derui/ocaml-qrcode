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
