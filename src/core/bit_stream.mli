type t
(** type of bit stream *)

(** type of result to read value from stream *)
type bit_result =
  | Continue of Type.Bit.t
  | Eos

val pp_bit_result : Format.formatter -> bit_result -> unit

val create : unit -> t
(** [create t] get new stream *)

val count : t -> int
(** [count t] get the bit count of stream *)

val put : bit:Type.Bit.t -> t -> t
(** [ put ~bit t] put a bit into [t] *)

val puts : data:Type.Bit.t list -> t -> t
(** [ put ~data t] put a bit into [t] *)

val put_int32 : data:int32 -> bits:int -> t -> t
(** [put_int32 ~data ~bits t] put integer [data] into [t] from data's [bits]-th bit to LSB. *)

val put_byte : data:Stdint.uint8 -> t -> t
(** [put_byte ~data t] put unsigned int 8 [data] into [t] *)

val next : t -> bit_result
(** [next t] get a next bit. If read position of stream is reached end of stream, return [Eos] *)

val concat : first:t -> last:t -> t
(** [concat ~first ~last] concatenate two bit streams [first] and [last]. Use [first] as first, then [last] as next of
    it. *)

val to_list : t -> Type.Bit.t list
(** [to_list t] get list contains all bit sequentially *)

val to_byte_list : t -> Stdint.uint8 list
(** [to_byte_list t] get list of byte that converted from [t] bit sequentially. If bit stream have reminder when divided
    by 8, last byte will irregular. *)

val clone : t -> t
(** [clone t] get cloned stream. A stream returned from this function is not related original stream, so user can get
    bits without affects to original stream *)
