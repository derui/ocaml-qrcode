(** All Version of QR Code model 2 *)
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

type capacity = {
  module_per_edge : int;
  function_pattern_module_count : int;
  type_and_version_module_count : int;
}

let to_capacity t =
  let edge v = 21 + (4 * v) in
  let small_version = 31 and large_version = 67 in
  match t with
  | V_1 ->
      { module_per_edge = edge 0; function_pattern_module_count = 202; type_and_version_module_count = small_version }
  | V_2 ->
      { module_per_edge = edge 1; function_pattern_module_count = 235; type_and_version_module_count = small_version }
  | V_3 ->
      { module_per_edge = edge 2; function_pattern_module_count = 243; type_and_version_module_count = small_version }
  | V_4 ->
      { module_per_edge = edge 3; function_pattern_module_count = 251; type_and_version_module_count = small_version }
  | V_5 ->
      { module_per_edge = edge 4; function_pattern_module_count = 259; type_and_version_module_count = small_version }
  | V_6 ->
      { module_per_edge = edge 5; function_pattern_module_count = 267; type_and_version_module_count = small_version }
  | V_7 ->
      { module_per_edge = edge 6; function_pattern_module_count = 390; type_and_version_module_count = large_version }
  | V_8 ->
      { module_per_edge = edge 7; function_pattern_module_count = 398; type_and_version_module_count = large_version }
  | V_9 ->
      { module_per_edge = edge 8; function_pattern_module_count = 406; type_and_version_module_count = large_version }
  | V_10 ->
      { module_per_edge = edge 9; function_pattern_module_count = 414; type_and_version_module_count = large_version }
  | V_11 ->
      { module_per_edge = edge 10; function_pattern_module_count = 422; type_and_version_module_count = large_version }
  | V_12 ->
      { module_per_edge = edge 11; function_pattern_module_count = 430; type_and_version_module_count = large_version }
  | V_13 ->
      { module_per_edge = edge 12; function_pattern_module_count = 438; type_and_version_module_count = large_version }
  | V_14 ->
      { module_per_edge = edge 13; function_pattern_module_count = 611; type_and_version_module_count = large_version }
  | V_15 ->
      { module_per_edge = edge 14; function_pattern_module_count = 619; type_and_version_module_count = large_version }
  | V_16 ->
      { module_per_edge = edge 15; function_pattern_module_count = 627; type_and_version_module_count = large_version }
  | V_17 ->
      { module_per_edge = edge 16; function_pattern_module_count = 635; type_and_version_module_count = large_version }
  | V_18 ->
      { module_per_edge = edge 17; function_pattern_module_count = 643; type_and_version_module_count = large_version }
  | V_19 ->
      { module_per_edge = edge 18; function_pattern_module_count = 651; type_and_version_module_count = large_version }
  | V_20 ->
      { module_per_edge = edge 19; function_pattern_module_count = 659; type_and_version_module_count = large_version }
  | V_21 ->
      { module_per_edge = edge 20; function_pattern_module_count = 882; type_and_version_module_count = large_version }
  | V_22 ->
      { module_per_edge = edge 21; function_pattern_module_count = 890; type_and_version_module_count = large_version }
  | V_23 ->
      { module_per_edge = edge 22; function_pattern_module_count = 898; type_and_version_module_count = large_version }
  | V_24 ->
      { module_per_edge = edge 23; function_pattern_module_count = 906; type_and_version_module_count = large_version }
  | V_25 ->
      { module_per_edge = edge 24; function_pattern_module_count = 914; type_and_version_module_count = large_version }
  | V_26 ->
      { module_per_edge = edge 25; function_pattern_module_count = 922; type_and_version_module_count = large_version }
  | V_27 ->
      { module_per_edge = edge 26; function_pattern_module_count = 930; type_and_version_module_count = large_version }
  | V_28 ->
      { module_per_edge = edge 27; function_pattern_module_count = 1203; type_and_version_module_count = large_version }
  | V_29 ->
      { module_per_edge = edge 28; function_pattern_module_count = 1211; type_and_version_module_count = large_version }
  | V_30 ->
      { module_per_edge = edge 29; function_pattern_module_count = 1219; type_and_version_module_count = large_version }
  | V_31 ->
      { module_per_edge = edge 30; function_pattern_module_count = 1227; type_and_version_module_count = large_version }
  | V_32 ->
      { module_per_edge = edge 31; function_pattern_module_count = 1235; type_and_version_module_count = large_version }
  | V_33 ->
      { module_per_edge = edge 32; function_pattern_module_count = 1243; type_and_version_module_count = large_version }
  | V_34 ->
      { module_per_edge = edge 33; function_pattern_module_count = 1251; type_and_version_module_count = large_version }
  | V_35 ->
      { module_per_edge = edge 34; function_pattern_module_count = 1574; type_and_version_module_count = large_version }
  | V_36 ->
      { module_per_edge = edge 35; function_pattern_module_count = 1582; type_and_version_module_count = large_version }
  | V_37 ->
      { module_per_edge = edge 36; function_pattern_module_count = 1590; type_and_version_module_count = large_version }
  | V_38 ->
      { module_per_edge = edge 37; function_pattern_module_count = 1598; type_and_version_module_count = large_version }
  | V_39 ->
      { module_per_edge = edge 38; function_pattern_module_count = 1606; type_and_version_module_count = large_version }
  | V_40 ->
      { module_per_edge = edge 39; function_pattern_module_count = 1614; type_and_version_module_count = large_version }

let to_data_capacity = function
  | V_1 -> 26
  | V_2 -> 44
  | V_3 -> 70
  | V_4 -> 100
  | V_5 -> 134
  | V_6 -> 172
  | V_7 -> 196
  | V_8 -> 242
  | V_9 -> 292
  | V_10 -> 346
  | V_11 -> 404
  | V_12 -> 466
  | V_13 -> 532
  | V_14 -> 581
  | V_15 -> 655
  | V_16 -> 733
  | V_17 -> 815
  | V_18 -> 901
  | V_19 -> 991
  | V_20 -> 1085
  | V_21 -> 1156
  | V_22 -> 1258
  | V_23 -> 1364
  | V_24 -> 1474
  | V_25 -> 1588
  | V_26 -> 1706
  | V_27 -> 1828
  | V_28 -> 1921
  | V_29 -> 2051
  | V_30 -> 2185
  | V_31 -> 2323
  | V_32 -> 2465
  | V_33 -> 2611
  | V_34 -> 2761
  | V_35 -> 2876
  | V_36 -> 3034
  | V_37 -> 3196
  | V_38 -> 3362
  | V_39 -> 3532
  | V_40 -> 3706
