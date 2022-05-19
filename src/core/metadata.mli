type t = private {
  version : Version.t;  (** version of selected QR code *)
  mode : Mode.t;  (** mode of selected QR code *)
  error_correction_level : Error_correction.level;  (** error correction level of selected QR code *)
  total_size : Stdint.Uint32.t;  (** total size of code word *)
  word_size : Stdint.Uint32.t;  (** size of code word is used for data *)
  bit_size : Stdint.Uint32.t;  (** bit size of code word is used for data *)
  data_size : Stdint.Uint32.t;  (** data size specified each mode *)
}

val make : version:Version.t -> mode:Mode.t -> error_correction_level:Error_correction.level -> t
(** [make ~version ~mode ~error_correction_level] make a metadata for version/mode/error correction level *)
