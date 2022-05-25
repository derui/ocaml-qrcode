type t = Stdint.uint8
(** type of symbol *)

val pp : Format.formatter -> t -> unit

val show : t -> string

val of_symbol : int -> t
(** [of_symbol coefficient] get symbol at [coefficient]. *)
