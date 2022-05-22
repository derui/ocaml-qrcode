(** This module provides functionally of symbol that is used in RS-coding.

    Symbol defined in this module is only GF(2^8). *)

type symbol = Stdint.uint8

let symbols =
  let open Stdint.Uint16 in
  let ( lor ) = logor in
  let ( & ) = logand in
  let sym = Array.make 256 zero in
  sym.(0) <- one;
  sym.(1) <- shift_left one 1;
  sym.(2) <- shift_left one 2;
  sym.(3) <- shift_left one 3;
  sym.(4) <- shift_left one 4;
  sym.(5) <- shift_left one 5;
  sym.(6) <- shift_left one 6;
  sym.(7) <- shift_left one 7;
  sym.(8) <- sym.(4) lor sym.(3) lor sym.(2) lor sym.(0);

  let mask = of_int 0b11111111 in

  Array.iteri
    (fun idx _ ->
      if idx < 9 then ()
      else
        let pre_idx = Stdlib.pred idx in
        let v = shift_left sym.(pre_idx) 1 in
        let v = logxor (shift_right v 8 * sym.(8)) (v & mask) in
        sym.(idx) <- v)
    sym;
  sym.(255) <- zero;
  Array.map Stdint.Uint8.of_uint16 sym

let of_symbol coefficient =
  if coefficient < 0 || coefficient >= Array.length symbols then
    raise (Invalid_argument (Printf.sprintf "Do not define %d in GF(2^8)" coefficient))
  else symbols.(coefficient)
