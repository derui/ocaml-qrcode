type word = Stdint.uint8

type t = private { words : word array }

val make : segments:Segment.t list -> metadata:Metadata.t -> t
(** [make ~segments ~metadata] concat [segments] on [metadata] as codewords. This codewords is used to calculate error
    correction codes. *)

val to_bit_stream : t -> Bit_stream.t
(** [to_bit_stream t] convert code words to [Bit_stream] *)
