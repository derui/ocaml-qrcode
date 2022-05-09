type t = {
  mode : Mode.t;
  count_indicator : Count_indicator.t;
  data : Bit_stream.t;
}

type encoding_error =
  | Invalid_data of string
  | Data_size_overflow of string * int

type encoded = (t, encoding_error) result

type data_generator = unit -> char option

module type S = sig
  val encode : metadata:Metadata.t -> generator:data_generator -> encoded
end
