type t = {
  mode : Mode.t;
  count_indicator : Count_indicator.t;
  data : Bit_stream.t;
}

type encoding_error =
  | Invalid_data of string
  | Data_size_overflow of string * int

type encoded = (t, encoding_error) result

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
end

let make ~mode ~version ~data ~size =
  let indicator = Count_indicator.of_mode_with_version ~mode ~version in
  let indicator = Count_indicator.set_count indicator ~count:size in
  { mode; count_indicator = indicator; data }
