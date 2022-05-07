type encoding_error =
  | Invalid_data of string
  | Data_size_overflow of string * int

type encoded = (Segment.t, encoding_error) result
(** result of encoding *)

type data = bytes
(** type of raw data that is encoded by encoder *)

type metadata = { version : Version.t }

module type Encoder = sig
  val mode : Mode.t

  val is_valid_data : char -> bool
  (** [is_valid_data byte] return a [byte] is valid data in this encoder *)

  val encode : stream:Bit_stream.t -> data:bytes -> Bit_stream.t
  (** [encode ~stream ~data] return given stream that encoder puts encoded data into *)
end

module type S = sig
  val encode : metadata -> bytes -> encoded
end
