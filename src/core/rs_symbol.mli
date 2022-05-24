type t = Stdint.uint8
(** type of symbol *)

val of_symbol : int -> t
(** [of_symbol coefficient] get symbol at [coefficient]. *)
