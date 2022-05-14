module S = Segment

let encode' ~metadata ~generator ~list_to_number_data =
  let stream = Bit_stream.create () in
  let max_size = Stdint.Uint32.to_int metadata.Metadata.word_size in
  match S.Support.read_data ~max_size generator with
  | None -> Error (S.Encoding_error.Data_size_overflow ("Can not accept size of data greater than", max_size))
  | Some data ->
      let open Std.Result.Let_syntax in
      let* number_list = list_to_number_data data in
      let stream =
        List.fold_left
          (fun stream (number, bits) ->
            S.Support.number_to_bit_list ~bits number
            |> List.fold_left (fun stream bit -> Bit_stream.put ~bit stream) stream)
          stream number_list
      in
      Ok (S.make ~mode:metadata.mode ~version:metadata.version ~data:stream ~size:(List.length data))

module Number : S.S = struct
  let to_number = function
    | '0' .. '9' as v -> Ok (Char.code v - Char.code '0')
    | _ as v -> Error (S.Encoding_error.Invalid_data (Printf.sprintf "Can not use character '%c' in number mode" v))

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

  let encode ~metadata ~generator = encode' ~metadata ~generator ~list_to_number_data
end

module Alphabet : S.S = struct
  let to_number = function
    | '0' .. '9' as v -> Ok (Char.code v - Char.code '0')
    | 'A' .. 'Z' as v -> Ok (Char.code v - Char.code 'A' + 10)
    | ' ' -> Ok 36
    | '$' -> Ok 37
    | '%' -> Ok 38
    | '*' -> Ok 39
    | '+' -> Ok 40
    | '-' -> Ok 41
    | '.' -> Ok 42
    | '/' -> Ok 43
    | ':' -> Ok 44
    | _ as v -> Error (S.Encoding_error.Invalid_data (Printf.sprintf "Can not use character '%c' in alphabet mode" v))

  let list_to_number_data list =
    let open Std.Result.Let_syntax in
    let rec loop accum = function
      | [] -> List.rev accum |> Result.ok
      | a :: b :: rest ->
          let* a = to_number a in
          let* b = to_number b in
          let v = (a * 45) + b in
          loop ((v, 11) :: accum) rest
      | [ a ] ->
          let* a = to_number a in
          loop ((a, 6) :: accum) []
    in
    loop [] list

  let encode ~metadata ~generator = encode' ~metadata ~generator ~list_to_number_data
end
