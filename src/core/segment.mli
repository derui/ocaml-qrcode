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

module Support : sig
  val read_data : max_size:int -> data_generator -> char list option
  (** [read_data ~max_size generator] read data from [generator] limit to [max_size]. If generator's internal maximum
      size is greater than [max_size], return None *)

  val number_to_bit_list : bits:int -> int -> [ `One | `Zero ] list
  (** [number_to_bit_list ~bits number] convert number into [bits]-sized bit list *)
end

val make : mode:Mode.t -> version:Version.t -> data:Bit_stream.t -> size:int -> t
(** [make ~mode ~version ~data ~size] make segment from data *)
