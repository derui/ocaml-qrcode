let () =
  Alcotest.run "OCaml QRCode core"
    [
      ("Bit Stream", Bit_stream_test.tests);
      ("Mode", Mode_test.tests);
      ("Segment encoders", Segment_encoders_test.tests);
      ("Segment decoders", Segment_decoders_test.tests);
      ("Segment", Segment_test.tests);
      ("Code word", Code_word_test.tests);
      ("RS coding symbol", Rs_symbol_test.tests);
      ("RS coding polynomial", Rs_polynomial_test.tests);
      ("Metadata", Metadata_test.tests);
      ("Format information", Format_information_test.tests);
      ("Version information", Version_information_test.tests);
      ("Type", Type_test.tests);
      ("Reed-Solomon Code", Reed_solomon_coding_test.tests);
    ]
