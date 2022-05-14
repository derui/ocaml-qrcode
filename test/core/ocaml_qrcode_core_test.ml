let () =
  Alcotest.run "OCaml QRCode core"
    [
      ("Bit Stream", Bit_stream_test.tests); ("Mode", Mode_test.tests); ("Segment encoders", Segment_encoders_test.tests);
    ]
