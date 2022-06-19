(* x^12 + x^11 + x^10 + x^9 + x^8 + x^5 + x^2 + 1 *)
let polynomial =
  [ `One; `One; `One; `One; `Zero; `Zero; `One; `Zero; `Zero; `One; `Zero; `One ] |> List.rev |> Array.of_list

let encode version =
  let bits = Version.to_bit version in
  match bits with
  | None -> None
  | Some bits ->
      let circuit bits =
        let k = 12 in
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
      Option.some @@ Bit_stream.puts ~data:format_information stream
