type t = private {
  version : Version.t;
  mode : Mode.t;
  error_correction_level : Error_correction.level;
  word_size : Stdint.Uint32.t;
  bit_size : Stdint.Uint32.t;
  data_size : Stdint.Uint32.t;
}

val make : version:Version.t -> mode:Mode.t -> error_correction_level:Error_correction.level -> t
(** [make ~version ~mode ~error_correction_level] make a metadata for version/mode/error correction level *)
