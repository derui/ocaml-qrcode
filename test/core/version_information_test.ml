module B = Ocaml_qrcode_core.Bit_stream
module VI = Ocaml_qrcode_core.Version_information
module V = Ocaml_qrcode_core.Version
open Support

let encode_test () =
  let encoded = VI.encode V.V_7 in
  let bit_string = encoded |> Option.map B.to_list |> Option.map to_bit_string in
  Alcotest.(check' @@ option string) ~msg:"bits" ~expected:(Some "000111110010010100") ~actual:bit_string

let no_version_information_test () =
  let encoded = VI.encode V.V_2 in
  let bit_string = encoded |> Option.map B.to_list |> Option.map to_bit_string in
  Alcotest.(check' @@ option string) ~msg:"bits" ~expected:None ~actual:bit_string

let tests =
  [
    Alcotest.test_case "can make information with error correction" `Quick encode_test;
    Alcotest.test_case "do not make information if version is lower than base" `Quick no_version_information_test;
  ]
