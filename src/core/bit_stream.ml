(** This module provides stream of bit. *)

type t = {
  mutable current_bits : int;
  mutable bits : int;
  mutable buffer : Stdint.Uint128.t array;
}

type bit =
  | One
  | Zero

type bit_result =
  | Continue of bit
  | Eos

let create () = { current_bits = 0; bits = 0; buffer = Array.make 1 Stdint.Uint128.zero }

let make_bitmask index =
  let shift = pred (Stdint.Uint128.bits - index) in
  Stdint.Uint128.(shift_left one shift |> max zero)

let expand_if_needed t =
  let total_bit_size = Array.length t.buffer * Stdint.Uint128.bits in
  if succ t.bits >= total_bit_size then
    let current_size = Array.length t.buffer in
    let buffer =
      Array.init (current_size * 2) (fun index ->
          if index < current_size then t.buffer.(index) else Stdint.Uint128.zero)
    in
    { t with buffer }
  else t

let put_data data t =
  let next_bits = succ t.bits in

  let t = expand_if_needed t in
  if data = Stdint.Uint128.one then (
    let reminder = next_bits mod Stdint.Uint128.bits in
    let target_bit = make_bitmask reminder in
    let index = if reminder > 0 then next_bits / Stdint.Uint128.bits else succ (next_bits / Stdint.Uint128.bits) in
    t.buffer.(index) <- Stdint.Uint128.logor t.buffer.(index) target_bit;
    { t with buffer = t.buffer; bits = next_bits })
  else { t with bits = next_bits }

let put ~data:bit t = match bit with One -> put_data Stdint.Uint128.one t | Zero -> put_data Stdint.Uint128.zero t

let next t =
  if t.current_bits >= t.bits then Eos
  else (
    t.current_bits <- succ t.current_bits;
    let index = t.current_bits / Stdint.Uint128.bits and reminder = t.current_bits mod Stdint.Uint128.bits in
    let bit_mask = make_bitmask reminder in
    let value = Stdint.Uint128.(logand t.buffer.(index) bit_mask) in
    if Stdint.Uint128.(compare zero value) = 0 then Continue Zero else Continue One)
