type t = {
  error_correction_level : Error_correction_level.t;
  mask_pattern : Mask.t;
}

let make ~error_correction_level ~mask_pattern = { error_correction_level; mask_pattern }

(* x^10 + x^8 + x^5 + x^4 + x^2 + x + 1 *)
let polynomial = [| `Zero; `One; `Zero; `Zero; `One; `One; `Zero; `One; `One; `One |]

(* index = 0 is LSB. *)
let mask = [ `One; `Zero; `One; `Zero; `One; `Zero; `Zero; `Zero; `Zero; `One; `Zero; `Zero; `One; `Zero ]

let to_bit_stream { error_correction_level; mask_pattern } =
  let indicator = Error_correction_level.to_indicator error_correction_level
  and pattern = Mask.to_reference mask_pattern in
  let bits = indicator @ pattern in

  let circuit bits =
    let k = 10 in
    let register = Array.make k `Zero in

    let put_datum datum =
      let module B = Type.Bit in
      let greater_index = k - 1 in
      let datum = B.(datum + register.(greater_index)) in
      Array.iteri
        (fun idx _ ->
          let coefficient = polynomial.(idx) in
          let previous_data = if idx = 0 then `Zero else register.(pred idx) in
          let module B = Type.Bit in
          register.(idx) <- B.((datum * coefficient) + previous_data))
        register
    in
    List.iter put_datum bits;

    register
  in
  let reminder = circuit bits |> Array.to_list |> List.rev in
  let format_information = bits @ reminder in
  let stream = Bit_stream.create () in
  let data =
    List.map2
      (fun b1 b2 ->
        let module B = Type.Bit in
        B.xor b1 b2)
      format_information mask
  in
  Bit_stream.puts ~data stream
