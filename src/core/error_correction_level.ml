type t =
  | Low
  | Medium
  | Quality
  | High  (** type of error correction level *)

let to_indicator = function
  | Low -> [ `Zero; `One ]
  | Medium -> [ `Zero; `Zero ]
  | Quality -> [ `One; `One ]
  | High -> [ `One; `Zero ]
