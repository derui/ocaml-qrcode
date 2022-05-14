type t =
  | Number
  | Alphabet
  | Byte  (** type of mode for QR Code *)

val show : t -> string

val pp : Format.formatter -> t -> unit

val to_bits : stream:Bit_stream.t -> t -> Bit_stream.t
(** [to_bits ~stream t] put bits that is as mode indicator into bit stream *)
