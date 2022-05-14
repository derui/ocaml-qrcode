let () =
  Alcotest.run "OCaml QRCode core"
    [
      ("Bit Stream", Bit_stream_test.tests);
      ("Mode", Mode_test.tests);
      ("Segment number encoder", Segment_number_encoder_test.tests);
    ]
