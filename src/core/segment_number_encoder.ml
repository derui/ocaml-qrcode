module S = Segment

let to_number = function
  | '0' .. '9' as v -> Ok (Char.code v - Char.code '0')
  | _ as v -> Error (S.Invalid_data (Printf.sprintf "Can not use character %c in number mode" v))

let number_to_bit_list ~bits number =
  let mask = Stdint.Uint32.(shift_right max_int bits |> lognot |> to_int) in
  let masked_number = number land mask in
  let rec loop count current_number accum =
    if count >= bits then accum
    else
      match current_number land 1 with
      | 1 -> loop (succ count) (current_number lsr 1) (`One :: accum)
      | _ -> loop (succ count) (current_number lsr 1) (`Zero :: accum)
  in

  loop 0 masked_number []

let list_to_number_data list =
  let open Std.Result.Let_syntax in
  let rec loop accum = function
    | [] -> List.rev accum |> Result.ok
    | a :: b :: c :: rest ->
        let* a = to_number a in
        let* b = to_number b in
        let* c = to_number c in
        let v = (a * 100) + (b * 10) + c in
        loop ((v, 10) :: accum) rest
    | [ a; b ] ->
        let* a = to_number a in
        let* b = to_number b in
        let v = (a * 10) + b in
        loop ((v, 7) :: accum) []
    | [ a ] ->
        let* a = to_number a in
        loop ((a, 4) :: accum) []
  in
  loop [] list

module Core : S.S = struct
  let encode ~metadata ~generator =
    let stream = Bit_stream.create () in
    let max_size = Stdint.Uint32.to_int metadata.Metadata.word_size in
    match S.Support.read_data ~max_size generator with
    | None -> Error (S.Data_size_overflow ("Can not accept size of data greater than", max_size))
    | Some data ->
        let open Std.Result.Let_syntax in
        let* number_list = list_to_number_data data in
        let stream =
          List.fold_left
            (fun stream (number, bits) ->
              number_to_bit_list ~bits number |> List.fold_left (fun stream bit -> Bit_stream.put ~bit stream) stream)
            stream number_list
        in
        Ok (S.make ~mode:metadata.mode ~version:metadata.version ~data:stream ~size:(List.length data))
end
