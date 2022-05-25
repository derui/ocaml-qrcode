type validated
(** a phantom type for polynomial number *)

type 'validated number
(** a phantom type for polynomial number *)

type t = Rs_symbol.t array
(** type of polynomial *)

val to_validated : int -> validated number option
(** [to_validated v] convert int[v] to validated value. If [v] is not valid in QR code's defined polynomial number,
    return [None] *)

val from_validated : validated number -> t
(** [from_validated v] get a polynomial from validated count *)
