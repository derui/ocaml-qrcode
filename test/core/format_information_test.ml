module B = Ocaml_qrcode_core.Bit_stream
module E = Ocaml_qrcode_core.Error_correction_level
module M = Ocaml_qrcode_core.Mask
module F = Ocaml_qrcode_core.Format_information
open Support

let encode_test () =
  let format = F.make ~error_correction_level:E.Medium ~mask_pattern:T_6 in
  let encoded = F.encode format in
  let bit_string = B.to_list encoded |> to_bit_string in
  Alcotest.(check' string) ~msg:"bits" ~expected:"100000011001110" ~actual:bit_string

let tests = [ Alcotest.test_case "can make information with error correction" `Quick encode_test ]
