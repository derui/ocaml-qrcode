type t = {
  bits : int;
  count : int;
}

let show { bits; count } = Printf.sprintf "{bits = %d, count = %d}" bits count

let pp fmt t = Format.fprintf fmt "%s" @@ show t

let make ~mode ~version =
  let bits =
    match mode with
    | Mode.Number -> (
        match version with
        | Version.V_1 | V_2 | V_3 | V_4 | V_5 | V_6 | V_7 | V_8 | V_9 -> 10
        | V_10 | V_11 | V_12 | V_13 | V_14 | V_15 | V_16 | V_17 | V_18 | V_19 | V_20 | V_21 | V_22 | V_23 | V_24 | V_25
        | V_26 ->
            12
        | _ -> 14)
    | Mode.Alphabet -> (
        match version with
        | Version.V_1 | V_2 | V_3 | V_4 | V_5 | V_6 | V_7 | V_8 | V_9 -> 9
        | V_10 | V_11 | V_12 | V_13 | V_14 | V_15 | V_16 | V_17 | V_18 | V_19 | V_20 | V_21 | V_22 | V_23 | V_24 | V_25
        | V_26 ->
            11
        | _ -> 13)
    | Mode.Byte -> ( match version with Version.V_1 | V_2 | V_3 | V_4 | V_5 | V_6 | V_7 | V_8 | V_9 -> 8 | _ -> 16)
  in
  { bits; count = 0 }

let set_count t ~count = { t with count }

let output_to_bit_stream ~stream t =
  let count = Int32.of_int t.count in
  Bit_stream.put_int32 ~data:count ~bits:t.bits stream
