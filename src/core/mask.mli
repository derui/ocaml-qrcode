type t =
  | T_1
  | T_2
  | T_3
  | T_4
  | T_5
  | T_6
  | T_7
  | T_8

val to_reference : t -> Type.Bit.t list
(** [to_reference mask] get a reference bit *)

val choice_applyable_mask : version:Version.t -> matrix:Module_matrix.t -> Type.Bit.t array array * t
(** [choice_applyable_mask ~version ~matrix] get a mask and masked matrix that is lowest conceded score *)
