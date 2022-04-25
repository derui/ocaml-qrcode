(** mode of QR Code data segment *)
module Mode = struct
  type t =
    | Number
    | Alphabet
    | Byte (* this is equal to 8-bit mode *)
end

(** All Version of QR Code model 2 *)
module Version = struct
  type t =
    | V_1
    | V_2
    | V_3
    | V_4
    | V_5
    | V_6
    | V_7
    | V_8
    | V_9
    | V_10
    | V_11
    | V_12
    | V_13
    | V_14
    | V_15
    | V_16
    | V_17
    | V_18
    | V_19
    | V_20
    | V_21
    | V_22
    | V_23
    | V_24
    | V_25
    | V_26
    | V_27
    | V_28
    | V_29
    | V_30
    | V_31
    | V_32
    | V_33
    | V_34
    | V_35
    | V_36
    | V_37
    | V_38
    | V_39
    | V_40
end

module Count_indicator = struct
  type t = {
    bits : int;
    count : int;
  }

  let of_mode_with_version ~mode ~version =
    let bits =
      match mode with
      | Mode.Number -> (
          match version with
          | Version.V_1 | V_2 | V_3 | V_4 | V_5 | V_6 | V_7 | V_8 | V_9 -> 10
          | V_10 | V_11 | V_12 | V_13 | V_14 | V_15 | V_16 | V_17 | V_18 | V_19 | V_20 | V_21 | V_22 | V_23 | V_24
          | V_25 | V_26 ->
              12
          | _ -> 14)
      | Mode.Alphabet -> (
          match version with
          | Version.V_1 | V_2 | V_3 | V_4 | V_5 | V_6 | V_7 | V_8 | V_9 -> 9
          | V_10 | V_11 | V_12 | V_13 | V_14 | V_15 | V_16 | V_17 | V_18 | V_19 | V_20 | V_21 | V_22 | V_23 | V_24
          | V_25 | V_26 ->
              11
          | _ -> 13)
      | Mode.Byte -> ( match version with Version.V_1 | V_2 | V_3 | V_4 | V_5 | V_6 | V_7 | V_8 | V_9 -> 8 | _ -> 16)
    in
    { bits; count = 0 }
end

module Segment = struct
  type t = {
    mode : Mode.t;
    count_indicator : Count_indicator.t;
  }
end
