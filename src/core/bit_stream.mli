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

val next : t -> bit_result
(** [next t] get a next bit. If read position of stream is reached end of stream, return [Eos] *)

val to_list : t -> bit list
(** [to_list t] get list contains all bit sequentially *)
