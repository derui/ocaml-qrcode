type t = {
  mode : Mode.t;
  count_indicator : Count_indicator.t;
  data : Bit_stream.t;
}

type encoding_error =
  | Invalid_data of string
  | Data_size_overflow of string * int  (** encoding error *)

type encoded = (t, encoding_error) result
(** type of encoding result *)

type data_generator = unit -> char option
(** type of data generator. Return None if no data *)

module type S = sig
  val encode : metadata:Metadata.t -> generator:data_generator -> encoded
  (** [encode ~metadata ~generator] get a segment from data generated from generator and metadata *)
end
