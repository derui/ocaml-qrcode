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

module GF256 = struct
  type t = Stdint.uint8

  let former = (1 lsl 4) + (1 lsl 3) + (1 lsl 2) + 1

  let galois =
    let module U = Stdint.Uint8 in
    let tbl = Hashtbl.create 256 in
    Hashtbl.add tbl (U.of_int 255) U.one;

    let values = Array.init 255 (fun v -> v) in
    let mask = 0xff in
    Array.fold_left
      (fun accum i ->
        Hashtbl.add tbl (U.of_int i) (U.of_int accum);

        let accum = accum lsl 1 in
        let lsb = accum lsr 8 in
        lsb * former lxor (accum land mask))
      1 values
    |> ignore;

    tbl

  let of_uint8 v = Hashtbl.find galois v

  let xor a b =
    let a = Hashtbl.find galois a in
    let b = Hashtbl.find galois b in

    Stdint.Uint8.logxor a b

  let multiply a b =
    let module U = Stdint.Uint8 in
    let c = U.to_int a + U.to_int b in

    let i = if c > 255 then c - 255 |> U.of_int else U.of_int c in
    Printf.printf "%d %s\n" c (U.to_string i);
    of_uint8 i

  let ( + ) = xor

  let ( * ) = multiply
end

type matrix = Bit.t array array

type position = int * int

module Position_set = Set.Make (struct
  type t = position

  let compare = Stdlib.compare
end)
