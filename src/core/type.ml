module Bit = struct
  type t =
    [ `Zero
    | `One
    ]

  let show = function `Zero -> "0" | `One -> "1"

  let pp fmt t = Format.fprintf fmt "%s" @@ show t

  let xor a b = match (a, b) with `Zero, `Zero -> `Zero | `One, `Zero | `Zero, `One -> `One | `One, `One -> `Zero

  let ( + ) = xor

  let ( * ) a b = match (a, b) with `Zero, `Zero | `One, `Zero | `Zero, `One -> `Zero | `One, `One -> `One
end
