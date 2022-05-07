module B = Bit_stream

(** mode of QR Code data segment *)
type t =
  | Number
  | Alphabet
  | Byte (* this is equal to 8-bit mode *)

let to_bits ~stream = function
  | Number -> B.puts ~data:[ `Zero; `Zero; `Zero; `One ] stream
  | Alphabet -> B.puts ~data:[ `Zero; `Zero; `One; `Zero ] stream
  | Byte -> B.puts ~data:[ `Zero; `One; `Zero; `Zero ] stream
