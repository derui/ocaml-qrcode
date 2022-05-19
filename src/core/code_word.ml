module B = Bit_stream

type word = Stdint.uint8

type t = { words : word array }

let termination_bits = List.init 4 (fun _ -> `Zero)

let reminder_codeword_1 = Stdint.Uint8.of_int 0b11101100

let reminder_codeword_2 = Stdint.Uint8.of_int 0b00010001

let fill_reminder_codewords ~stream_count ~metadata ~codewords =
  let current_code_word_count = stream_count / 8 |> Stdint.Uint32.of_int in
  let needing_fill_code_word_count = Stdint.Uint32.(metadata.Metadata.word_size - current_code_word_count |> to_int) in

  let reminder_codewords =
    List.init needing_fill_code_word_count (fun i -> if i / 2 = 0 then reminder_codeword_1 else reminder_codeword_2)
  in
  codewords @ reminder_codewords

let make ~segments ~metadata =
  let stream = Bit_stream.create () in
  let stream = List.fold_left (fun stream segment -> Segment.output_to_bit_stream ~stream segment) stream segments in
  let stream = B.puts ~data:termination_bits stream in
  let stream_count = B.count stream in
  let reminder_bits = stream_count mod 8 in

  let data = List.init reminder_bits (fun _ -> `Zero) in
  let stream = B.puts ~data stream in
  let codewords = B.to_byte_list stream in
  fill_reminder_codewords ~stream_count ~metadata ~codewords
