open Ocaml_qrcode_core
module M = Metadata

let reminder_bit_count_test () =
  let module S = Support in
  let for_v version =
    M.make ~version ~mode:Mode.Byte ~error_correction_level:Error_correction.Low
    |> M.reminder_bit_count |> Stdint.Uint8.to_int
  in
  let open Version in
  Alcotest.(check' int) ~msg:"version 1" ~actual:(for_v V_1) ~expected:0;
  Alcotest.(check' int) ~msg:"version 2" ~actual:(for_v V_2) ~expected:7;
  Alcotest.(check' int) ~msg:"version 3" ~actual:(for_v V_3) ~expected:7;
  Alcotest.(check' int) ~msg:"version 4" ~actual:(for_v V_4) ~expected:7;
  Alcotest.(check' int) ~msg:"version 5" ~actual:(for_v V_5) ~expected:7;
  Alcotest.(check' int) ~msg:"version 6" ~actual:(for_v V_6) ~expected:7;
  Alcotest.(check' int) ~msg:"version 7" ~actual:(for_v V_7) ~expected:0;
  Alcotest.(check' int) ~msg:"version 8" ~actual:(for_v V_8) ~expected:0;
  Alcotest.(check' int) ~msg:"version 9" ~actual:(for_v V_9) ~expected:0;
  Alcotest.(check' int) ~msg:"version 10" ~actual:(for_v V_10) ~expected:0;
  Alcotest.(check' int) ~msg:"version 11" ~actual:(for_v V_11) ~expected:0;
  Alcotest.(check' int) ~msg:"version 12" ~actual:(for_v V_12) ~expected:0;
  Alcotest.(check' int) ~msg:"version 13" ~actual:(for_v V_13) ~expected:0;
  Alcotest.(check' int) ~msg:"version 14" ~actual:(for_v V_14) ~expected:3;
  Alcotest.(check' int) ~msg:"version 15" ~actual:(for_v V_15) ~expected:3;
  Alcotest.(check' int) ~msg:"version 16" ~actual:(for_v V_16) ~expected:3;
  Alcotest.(check' int) ~msg:"version 17" ~actual:(for_v V_17) ~expected:3;
  Alcotest.(check' int) ~msg:"version 18" ~actual:(for_v V_18) ~expected:3;
  Alcotest.(check' int) ~msg:"version 19" ~actual:(for_v V_19) ~expected:3;
  Alcotest.(check' int) ~msg:"version 20" ~actual:(for_v V_20) ~expected:3;
  Alcotest.(check' int) ~msg:"version 21" ~actual:(for_v V_21) ~expected:4;
  Alcotest.(check' int) ~msg:"version 22" ~actual:(for_v V_22) ~expected:4;
  Alcotest.(check' int) ~msg:"version 23" ~actual:(for_v V_23) ~expected:4;
  Alcotest.(check' int) ~msg:"version 24" ~actual:(for_v V_24) ~expected:4;
  Alcotest.(check' int) ~msg:"version 25" ~actual:(for_v V_25) ~expected:4;
  Alcotest.(check' int) ~msg:"version 26" ~actual:(for_v V_26) ~expected:4;
  Alcotest.(check' int) ~msg:"version 27" ~actual:(for_v V_27) ~expected:4;
  Alcotest.(check' int) ~msg:"version 28" ~actual:(for_v V_28) ~expected:3;
  Alcotest.(check' int) ~msg:"version 29" ~actual:(for_v V_29) ~expected:3;
  Alcotest.(check' int) ~msg:"version 20" ~actual:(for_v V_30) ~expected:3;
  Alcotest.(check' int) ~msg:"version 31" ~actual:(for_v V_31) ~expected:3;
  Alcotest.(check' int) ~msg:"version 32" ~actual:(for_v V_32) ~expected:3;
  Alcotest.(check' int) ~msg:"version 33" ~actual:(for_v V_33) ~expected:3;
  Alcotest.(check' int) ~msg:"version 34" ~actual:(for_v V_34) ~expected:3;
  Alcotest.(check' int) ~msg:"version 35" ~actual:(for_v V_35) ~expected:0;
  Alcotest.(check' int) ~msg:"version 36" ~actual:(for_v V_36) ~expected:0;
  Alcotest.(check' int) ~msg:"version 37" ~actual:(for_v V_37) ~expected:0;
  Alcotest.(check' int) ~msg:"version 38" ~actual:(for_v V_38) ~expected:0;
  Alcotest.(check' int) ~msg:"version 39" ~actual:(for_v V_39) ~expected:0;
  Alcotest.(check' int) ~msg:"version 40" ~actual:(for_v V_40) ~expected:0

let need_version_info_test () =
  let module S = Support in
  let for_v version =
    M.make ~version ~mode:Mode.Byte ~error_correction_level:Error_correction.Low |> M.need_version_information
  in
  Alcotest.(check' bool) ~msg:"Not need on V1" ~actual:(for_v Version.V_1) ~expected:false;
  Alcotest.(check' bool) ~msg:"Not need on V2" ~actual:(for_v Version.V_2) ~expected:false;
  Alcotest.(check' bool) ~msg:"Not need on V6" ~actual:(for_v Version.V_6) ~expected:false;
  Alcotest.(check' bool) ~msg:"Need on V7" ~actual:(for_v Version.V_7) ~expected:true;
  Alcotest.(check' bool) ~msg:"Need on V40" ~actual:(for_v Version.V_40) ~expected:true

let tests =
  [
    Alcotest.test_case "calculate reminder bit" `Quick reminder_bit_count_test;
    Alcotest.test_case "need version information" `Quick need_version_info_test;
  ]
