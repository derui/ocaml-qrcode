module B = Bit_stream

type word = Stdint.uint8

type t = { words : word array }

let termination_bits count = List.init count (fun _ -> `Zero)

let reminder_codeword_1 = Stdint.Uint8.of_int 0b11101100

let reminder_codeword_2 = Stdint.Uint8.of_int 0b00010001

let fill_reminder_codewords ~metadata ~codewords =
  let current_code_word_count = List.length codewords |> Stdint.Uint32.of_int in
  let needing_fill_code_word_count = Stdint.Uint32.(metadata.Metadata.word_size - current_code_word_count |> to_int) in
  let reminder_codewords =
    List.init needing_fill_code_word_count (fun i -> if i mod 2 = 0 then reminder_codeword_1 else reminder_codeword_2)
  in
  codewords @ reminder_codewords

let add_reminder_bits_if_necessary metadata stream =
  let bit_count = B.count stream |> Stdint.Uint32.of_int in
  let lack_bits = Stdint.Uint32.(metadata.Metadata.bit_size - bit_count |> to_int) in
  let termination_bits = if lack_bits >= 4 then termination_bits 4 else termination_bits lack_bits in
  B.puts ~data:termination_bits stream

let make ~segments ~metadata =
  let stream = B.create () in
  let stream = List.fold_left (fun stream segment -> Segment.output_to_bit_stream ~stream segment) stream segments in
  let stream = add_reminder_bits_if_necessary metadata stream in
  let stream_count = B.count stream in
  let reminder_bits =
    let reminder = stream_count mod 8 in
    if reminder > 0 then 8 - reminder else 0
  in

  let data = List.init reminder_bits (fun _ -> `Zero) in
  let stream = B.puts ~data stream in
  let codewords = B.to_byte_list stream in
  { words = fill_reminder_codewords ~metadata ~codewords |> Array.of_list }

let to_bit_stream { words } =
  let stream = B.create () in
  Array.fold_left (fun stream word -> B.put_byte ~data:word stream) stream words
