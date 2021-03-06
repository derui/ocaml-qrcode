type t = private {
  bits : int;
  count : int;
}
(** type of count indicator *)

val show : t -> string

val pp : Format.formatter -> t -> unit

val make : mode:Mode.t -> version:Version.t -> t
(** [make ~mode ~version] make count indicator *)

val set_count : t -> count:int -> t
(** [set_count t ~count] get new indicator with [count] *)

val output_to_bit_stream : stream:Bit_stream.t -> t -> Bit_stream.t
(** [output_to_bit_stream ~stream t] output segment's bit sequence into [stream]. *)
