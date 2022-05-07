(** mode of QR Code data segment *)
type t =
  | Number
  | Alphabet
  | Byte (* this is equal to 8-bit mode *)
