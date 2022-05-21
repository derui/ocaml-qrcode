type word = Stdint.uint8

type t = private { words : word array }

val make : segments:Segment.t list -> metadata:Metadata.t -> t
(** [make ~segments ~metadata] concat [segments] on [metadata] as codewords. This codewords is used to calculate error
    correction codes. *)
