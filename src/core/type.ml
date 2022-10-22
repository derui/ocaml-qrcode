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

  let to_int = function `Zero -> 0 | `One -> 1
end

module GF256 = struct
  let former = (1 lsl 4) + (1 lsl 3) + (1 lsl 2) + 1

  let galois =
    let module U = Stdint.Uint8 in
    let exponent = Hashtbl.create 256 in
    let power = Hashtbl.create 256 in
    Hashtbl.add exponent (U.of_int 255) U.one;

    let values = Array.init 255 (fun v -> v) in
    let mask = 0xff in
    Array.fold_left
      (fun accum i ->
        Hashtbl.add exponent (U.of_int i) (U.of_int accum);
        Hashtbl.add power (U.of_int accum) (U.of_int i);

        let accum = accum lsl 1 in
        let lsb = accum lsr 8 in
        lsb * former lxor (accum land mask))
      1 values
    |> ignore;

    (exponent, power)

  let xor a b = Stdint.Uint8.logxor a b

  let multiply a b =
    let module U = Stdint.Uint8 in
    let a' = Hashtbl.find (snd galois) a |> U.to_int and b' = Hashtbl.find (snd galois) b |> U.to_int in
    let c = (a' + b') mod 255 in
    Hashtbl.find (fst galois) @@ U.of_int c

  let ( + ) = xor

  let ( * ) = multiply
end

type matrix = Bit.t array array

type position = int * int

module Position_set = Set.Make (struct
  type t = position

  let compare = Stdlib.compare
end)
