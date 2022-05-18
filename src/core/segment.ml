type t = {
  mode : Mode.t;
  count_indicator : Count_indicator.t;
  data : Bit_stream.t;
}

module Encoding_error = struct
  type t =
    | Invalid_data of string
    | Data_size_overflow of string * int

  let show = function
    | Invalid_data data -> Printf.sprintf "Invalid_data(%s)" data
    | Data_size_overflow (msg, size) -> Printf.sprintf "Data_size_overflow(msg = %s, size = %d)" msg size

  let pp fmt t = Format.fprintf fmt "%s" @@ show t
end

type encoded = (t, Encoding_error.t) result

type data_generator = unit -> char option

module type S = sig
  val encode : metadata:Metadata.t -> generator:data_generator -> encoded
end

module Support = struct
  let read_data ~max_size generator =
    if max_size < 0 then raise (Invalid_argument "max size must be greater equal zero")
    else
      let rec loop count accum =
        if count > max_size then None
        else match generator () with None -> List.rev accum |> Option.some | Some c -> loop (succ count) (c :: accum)
      in
      loop 0 []

  let number_to_bit_list ~bits number =
    let rec loop count current_number accum =
      if count >= bits then accum
      else
        match current_number land 1 with
        | 1 -> loop (succ count) (current_number lsr 1) (`One :: accum)
        | _ -> loop (succ count) (current_number lsr 1) (`Zero :: accum)
    in

    loop 0 number []
end

let make ~mode ~version ~data ~size =
  let indicator = Count_indicator.make ~mode ~version |> Count_indicator.set_count ~count:size in
  { mode; count_indicator = indicator; data }

let output_to_bit_stream t ~stream =
  let stream = Mode.to_bits ~stream t.mode in
  let stream = Count_indicator.output_to_bit_stream ~stream t.count_indicator in
  Bit_stream.concat ~first:stream ~last:t.data
