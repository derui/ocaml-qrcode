module S = Segment

module Number : S.Dec = struct
  let decode (t : Segment.t) =
    assert (t.mode = Mode.Number);
    let bit_list = Bit_stream.to_list t.data in
    let bit_size = List.length bit_list in
    let full_data_count = bit_size / 10 and rest_bit = bit_size mod 10 in
    let to_char v = Char.chr (Char.code '0' + v) in
    let bit_stream = Bit_stream.clone t.data in

    let rec loop count accum =
      if count <= 0 then accum
      else
        let data = Bit_stream.next_int32 ~size:10 bit_stream |> Int32.to_int in
        let a = data / 100 in
        let b = data mod 100 / 10 in
        let c = data mod 10 in
        loop (pred count) (c :: b :: a :: accum)
    in
    let data = loop full_data_count [] in
    let rest_data = Bit_stream.next_int32 ~size:rest_bit bit_stream |> Int32.to_int in

    let data =
      match rest_bit with
      | 7 ->
          let a = rest_data / 10 and b = rest_data mod 10 in
          b :: a :: data
      | 4 -> rest_data :: data
      | _ -> failwith "Invalid data"
    in
    List.rev data |> List.map to_char
end

module Alphabet : S.Dec = struct
  let to_char = function
    | v when v >= 0 && v <= 9 -> Char.chr (v + Char.code '0')
    | v when v >= 10 && v <= 35 -> Char.chr (v - 10 + Char.code 'A')
    | 36 -> ' '
    | 37 -> '$'
    | 38 -> '%'
    | 39 -> '*'
    | 40 -> '+'
    | 41 -> '-'
    | 42 -> '.'
    | 43 -> '/'
    | 44 -> ':'
    | _ -> failwith "Invalid argument"

  let decode (t : Segment.t) =
    assert (t.mode = Mode.Alphabet);
    let bit_list = Bit_stream.to_list t.data in
    let bit_size = List.length bit_list in
    let full_data_count = bit_size / 11 and rest_bit = bit_size mod 11 in
    let bit_stream = Bit_stream.clone t.data in

    let rec loop count accum =
      if count <= 0 then accum
      else
        let data = Bit_stream.next_int32 ~size:11 bit_stream |> Int32.to_int in
        let a = data / 45 in
        let b = data mod 45 in
        loop (pred count) (b :: a :: accum)
    in
    let data = loop full_data_count [] in
    let rest_data = Bit_stream.next_int32 ~size:rest_bit bit_stream |> Int32.to_int in

    let data = rest_data :: data in
    List.rev data |> List.map to_char
end

module Byte : S.Dec = struct
  let decode _ = failwith ""
end
