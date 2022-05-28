type t = {
  version : Version.t;
  mode : Mode.t;
  error_correction_level : Error_correction.level;
  total_size : Stdint.Uint32.t;
  word_size : Stdint.Uint32.t;
  bit_size : Stdint.Uint32.t;
  data_size : Stdint.Uint32.t;
}

let to_word_size ~version ~error_correction_level =
  let open Version in
  let module E = Error_correction in
  match (version, error_correction_level) with
  | V_1, E.Low -> 19
  | V_1, Medium -> 16
  | V_1, Quality -> 13
  | V_1, High -> 9
  | V_2, Low -> 34
  | V_2, Medium -> 28
  | V_2, Quality -> 22
  | V_2, High -> 16
  | V_3, Low -> 55
  | V_3, Medium -> 44
  | V_3, Quality -> 32
  | V_3, High -> 26
  | V_4, Low -> 80
  | V_4, Medium -> 64
  | V_4, Quality -> 48
  | V_4, High -> 36
  | V_5, Low -> 108
  | V_5, Medium -> 86
  | V_5, Quality -> 62
  | V_5, High -> 46
  | V_6, Low -> 136
  | V_6, Medium -> 108
  | V_6, Quality -> 76
  | V_6, High -> 60
  | V_7, Low -> 156
  | V_7, Medium -> 124
  | V_7, Quality -> 88
  | V_7, High -> 66
  | V_8, Low -> 194
  | V_8, Medium -> 154
  | V_8, Quality -> 110
  | V_8, High -> 86
  | V_9, Low -> 232
  | V_9, Medium -> 182
  | V_9, Quality -> 132
  | V_9, High -> 100
  | V_10, Low -> 274
  | V_10, Medium -> 216
  | V_10, Quality -> 154
  | V_10, High -> 122
  | V_11, Low -> 324
  | V_11, Medium -> 254
  | V_11, Quality -> 180
  | V_11, High -> 140
  | V_12, Low -> 370
  | V_12, Medium -> 290
  | V_12, Quality -> 206
  | V_12, High -> 158
  | V_13, Low -> 428
  | V_13, Medium -> 334
  | V_13, Quality -> 244
  | V_13, High -> 180
  | V_14, Low -> 461
  | V_14, Medium -> 365
  | V_14, Quality -> 261
  | V_14, High -> 197
  | V_15, Low -> 523
  | V_15, Medium -> 415
  | V_15, Quality -> 295
  | V_15, High -> 223
  | V_16, Low -> 589
  | V_16, Medium -> 453
  | V_16, Quality -> 325
  | V_16, High -> 253
  | V_17, Low -> 647
  | V_17, Medium -> 507
  | V_17, Quality -> 367
  | V_17, High -> 283
  | V_18, Low -> 721
  | V_18, Medium -> 563
  | V_18, Quality -> 397
  | V_18, High -> 313
  | V_19, Low -> 795
  | V_19, Medium -> 627
  | V_19, Quality -> 445
  | V_19, High -> 341
  | V_20, Low -> 861
  | V_20, Medium -> 669
  | V_20, Quality -> 485
  | V_20, High -> 385
  | V_21, Low -> 932
  | V_21, Medium -> 714
  | V_21, Quality -> 512
  | V_21, High -> 406
  | V_22, Low -> 1006
  | V_22, Medium -> 782
  | V_22, Quality -> 568
  | V_22, High -> 442
  | V_23, Low -> 1094
  | V_23, Medium -> 860
  | V_23, Quality -> 614
  | V_23, High -> 462
  | V_24, Low -> 1174
  | V_24, Medium -> 914
  | V_24, Quality -> 664
  | V_24, High -> 514
  | V_25, Low -> 1276
  | V_25, Medium -> 1000
  | V_25, Quality -> 718
  | V_25, High -> 538
  | V_26, Low -> 1370
  | V_26, Medium -> 1062
  | V_26, Quality -> 754
  | V_26, High -> 596
  | V_27, Low -> 1468
  | V_27, Medium -> 1128
  | V_27, Quality -> 808
  | V_27, High -> 628
  | V_28, Low -> 1531
  | V_28, Medium -> 1193
  | V_28, Quality -> 871
  | V_28, High -> 661
  | V_29, Low -> 1631
  | V_29, Medium -> 1267
  | V_29, Quality -> 911
  | V_29, High -> 701
  | V_30, Low -> 1735
  | V_30, Medium -> 1373
  | V_30, Quality -> 985
  | V_30, High -> 745
  | V_31, Low -> 1843
  | V_31, Medium -> 1455
  | V_31, Quality -> 1033
  | V_31, High -> 793
  | V_32, Low -> 1955
  | V_32, Medium -> 1541
  | V_32, Quality -> 1115
  | V_32, High -> 845
  | V_33, Low -> 2071
  | V_33, Medium -> 1631
  | V_33, Quality -> 1171
  | V_33, High -> 901
  | V_34, Low -> 2191
  | V_34, Medium -> 1725
  | V_34, Quality -> 1231
  | V_34, High -> 961
  | V_35, Low -> 2306
  | V_35, Medium -> 1812
  | V_35, Quality -> 1286
  | V_35, High -> 986
  | V_36, Low -> 2434
  | V_36, Medium -> 1914
  | V_36, Quality -> 1354
  | V_36, High -> 1054
  | V_37, Low -> 2566
  | V_37, Medium -> 1992
  | V_37, Quality -> 1426
  | V_37, High -> 1096
  | V_38, Low -> 2702
  | V_38, Medium -> 2102
  | V_38, Quality -> 1502
  | V_38, High -> 1142
  | V_39, Low -> 2812
  | V_39, Medium -> 2216
  | V_39, Quality -> 1582
  | V_39, High -> 1222
  | V_40, Low -> 2956
  | V_40, Medium -> 2334
  | V_40, Quality -> 1666
  | V_40, High -> 1276

let to_number_data_size data_bit_size =
  let characters = data_bit_size / 10 * 3 in
  let reminder = match data_bit_size mod 10 with 7 | 8 | 9 -> 2 | 4 | 5 | 6 -> 1 | _ -> 0 in
  characters + reminder

let to_alphabet_data_size data_bit_size =
  let characters = data_bit_size / 11 * 2 in
  let reminder = match data_bit_size mod 11 with 7 | 8 | 9 | 10 -> 1 | _ -> 0 in
  characters + reminder

let make ~version ~mode ~error_correction_level =
  let total_size = Version.to_data_capacity version in
  let word_size = to_word_size ~version ~error_correction_level in
  let bit_size = word_size * 8 in
  let indicator_bits =
    let v = Count_indicator.make ~mode ~version in
    v.bits
  in
  let data_bit_size = bit_size - indicator_bits - Mode.bit_size in
  let data_size =
    match mode with
    | Mode.Number -> to_number_data_size data_bit_size
    | Mode.Alphabet -> to_alphabet_data_size data_bit_size
    | Mode.Byte -> data_bit_size / 8
  in
  {
    version;
    mode;
    error_correction_level;
    total_size = Stdint.Uint32.of_int total_size;
    word_size = Stdint.Uint32.of_int word_size;
    bit_size = Stdint.Uint32.of_int bit_size;
    data_size = Stdint.Uint32.of_int data_size;
  }

let reminder_bit_count { version; _ } =
  let capacity = Version.to_capacity version in
  let all_modules =
    (capacity.module_per_edge * capacity.module_per_edge)
    - capacity.function_pattern_module_count - capacity.type_and_version_module_count
  in
  all_modules mod 8 |> Stdint.Uint8.of_int
