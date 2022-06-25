module C = Ocaml_qrcode_core
module E = Ocaml_qrcode_encoder

let () =
  let base_data = read_line () in
  let data = base_data |> String.to_seq |> Seq.map Char.code |> Seq.map Stdint.Uint8.of_int in
  let metadata =
    C.Metadata.make ~version:C.Version.V_1 ~mode:C.Mode.Byte ~error_correction_level:C.Error_correction_level.Medium
  in
  let function_module = C.Function_module.make metadata in

  (* make segment from data *)
  let bit_stream = C.Bit_stream.create () in
  let bit_stream = Seq.fold_left (fun bs byte -> C.Bit_stream.put_byte ~data:byte bs) bit_stream data in
  let segment =
    C.Segment.make ~mode:metadata.mode ~version:metadata.version ~data:bit_stream ~size:(String.length base_data)
  in

  (* make error correction blocks *)
  let code_words = C.Code_word.make ~segments:[ segment ] ~metadata in
  let blocks = C.Reed_solomon_coding.calculate_ec ~metadata code_words in
  let matrix = C.Module_matrix.Writer.make_matrix ~metadata ~function_module ~blocks in
  let matrix_data, mask =
    C.Mask.choice_applyable_mask ~version:metadata.version ~function_module:matrix.function_module ~matrix:matrix.matrix
  in
  let format_information =
    C.Format_information.make ~error_correction_level:metadata.error_correction_level ~mask_pattern:mask
  in
  (* let matrix = C.Module_matrix.{ matrix with matrix = matrix_data } in *)
  let matrix = C.Module_matrix.Writer.write_format_information ~format_information ~metadata matrix in
  let matrix = C.Module_matrix.Writer.write_version_information ~metadata matrix in

  (* write png *)
  E.Png_encoder.encode ~path:"/tmp/code.png" ~matrix ~options:{ pixel_per_module = 8 }
