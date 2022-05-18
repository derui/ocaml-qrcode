type t
(** type of bit stream *)

type bit =
  [ `One
  | `Zero
  ]
(** type of value in stream *)

val show_bit : bit -> string

val pp_bit : Format.formatter -> bit -> unit

(** type of result to read value from stream *)
type bit_result =
  | Continue of bit
  | Eos

val pp_bit_result : Format.formatter -> bit_result -> unit

val create : unit -> t
(** [create t] get new stream *)

val put : bit:bit -> t -> t
(** [ put ~bit t] put a bit into [t] *)

val puts : data:bit list -> t -> t
(** [ put ~data t] put a bit into [t] *)

val put_int32 : data:int32 -> bits:int -> t -> t
(** [put_int32 ~data ~bits t] put integer [data] into [t] from data's [bits]-th bit to LSB. *)

val next : t -> bit_result
(** [next t] get a next bit. If read position of stream is reached end of stream, return [Eos] *)

val concat : first:t -> last:t -> t
(** [concat ~first ~last] concatenate two bit streams [first] and [last]. Use [first] as first, then [last] as next of
    it. *)

val to_list : t -> bit list
(** [to_list t] get list contains all bit sequentially *)

val clone : t -> t
(** [clone t] get cloned stream. A stream returned from this function is not related original stream, so user can get
    bits without affects to original stream *)
