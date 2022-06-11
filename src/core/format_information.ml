type t = {
  error_correction_level : Error_correction_level.t;
  mask_pattern : Mask.t;
}

let make ~error_correction_level ~mask_pattern = { error_correction_level; mask_pattern }

(* x^10 + x^8 + x^5 + x^4 + x^2 + x + 1 *)
let polynomial = [ `Zero; `One; `Zero; `Zero; `One; `One; `Zero; `One; `One; `One ] |> List.rev |> Array.of_list

(* index = 0 is LSB. *)
let mask = [ `One; `Zero; `One; `Zero; `One; `Zero; `Zero; `Zero; `Zero; `Zero; `One; `Zero; `Zero; `One; `Zero ]

let encode { error_correction_level; mask_pattern } =
  let indicator = Error_correction_level.to_indicator error_correction_level
  and pattern = Mask.to_reference mask_pattern in
  let bits = indicator @ pattern in
  Printf.printf "%d\n" @@ List.length mask;

  let circuit bits =
    let k = 10 in
    let greater_index = k - 1 in
    let register = Array.make k `Zero in
    let prev = ref `Zero in

    let shift bit =
      let module B = Type.Bit in
      let feedback = register.(greater_index) in

      Array.iteri
        (fun idx _ ->
          let coefficient = polynomial.(idx) in
          let previous_data = if idx = 0 then bit else !prev in
          let module B = Type.Bit in
          prev := register.(idx);
          register.(idx) <- B.((feedback * coefficient) + previous_data))
        register
    in
    List.iter shift bits;
    Array.iter shift @@ Array.make k `Zero;

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
