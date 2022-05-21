(** This module provides functions to create data, use testable in test. *)

open Ocaml_qrcode_core

val data_to_generator : 'a list -> unit -> 'a option

val to_bit_string : [< `One | `Zero ] list -> String.t

val error_testable : Segment.Encoding_error.t Alcotest.testable

val mode_testable : (Mode.t, Segment.Encoding_error.t) result Alcotest.testable

val count_indicator_testable : (Ocaml_qrcode_core.Count_indicator.t, Segment.Encoding_error.t) result Alcotest.testable

val code_word_testable : Ocaml_qrcode_core.Code_word.word array Alcotest.testable
