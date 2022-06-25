type t =
  | T_1
  | T_2
  | T_3
  | T_4
  | T_5
  | T_6
  | T_7
  | T_8

let to_reference = function
  | T_1 -> [ `Zero; `Zero; `Zero ]
  | T_2 -> [ `Zero; `Zero; `One ]
  | T_3 -> [ `Zero; `One; `Zero ]
  | T_4 -> [ `Zero; `One; `One ]
  | T_5 -> [ `One; `Zero; `Zero ]
  | T_6 -> [ `One; `Zero; `One ]
  | T_7 -> [ `One; `One; `Zero ]
  | T_8 -> [ `One; `One; `One ]

let to_mask_pattern ~version t =
  let predicate =
    match t with
    | T_1 -> fun row col -> (row + col) mod 2 = 0
    | T_2 -> fun row _ -> row mod 2 = 0
    | T_3 -> fun _ col -> col mod 2 = 0
    | T_4 -> fun row col -> (row + col) mod 3 = 0
    | T_5 -> fun row col -> ((row / 2) + (col / 3)) mod 2 = 0
    | T_6 -> fun row col -> (col * row mod 2) + (col * row mod 3) = 0
    | T_7 -> fun row col -> ((row * col mod 2) + (row * col mod 3)) mod 2 = 0
    | T_8 -> fun row col -> (((row + col) mod 2) + (row * col mod 3)) mod 2 = 0
  in
  let edge =
    let capacity = Version.to_capacity version in
    capacity.module_per_edge
  in
  Array.init edge (fun row -> Array.init edge (fun col -> if predicate row col then `One else `Zero))

module Conceded_score = struct
  let for_adjacent matrix =
    let rec adjacent_row row accum =
      if row >= Array.length matrix then accum
      else
        let score = ref 0 in
        let repeated = ref 0 in
        let prev = ref None in
        Array.iter
          (fun v ->
            match !prev with
            | None -> prev := Some v
            | Some prev_bit ->
                if prev_bit = v then incr repeated
                else if !repeated >= 5 then (
                  score := !score + (!repeated - 2);
                  repeated := 0)
                else ();
                prev := Some v)
          matrix.(row);
        adjacent_row (succ row) (accum + !score)
    in
    let rec adjacent_col col accum =
      if col >= Array.length matrix then accum
      else
        let score = ref 0 in
        let repeated = ref 0 in
        let prev = ref None in
        Array.iter
          (fun row ->
            let v = row.(col) in
            match !prev with
            | None -> prev := Some v
            | Some prev_bit ->
                if prev_bit = v then incr repeated
                else if !repeated >= 5 then (
                  score := !score + (!repeated - 2);
                  repeated := 0)
                else ();
                prev := Some v)
          matrix;
        adjacent_col (succ col) (accum + !score)
    in
    let row_score = adjacent_row 0 0 and col_score = adjacent_col 0 0 in
    row_score + col_score

  let for_module_block matrix =
    let size = Array.length matrix in
    let score = 3 in
    let rec calculate_block row col accum =
      if row >= size - 1 then accum
      else if col >= size - 1 then calculate_block (succ row) 0 accum
      else
        let v1 = matrix.(row).(col)
        and v2 = matrix.(row + 1).(col)
        and v3 = matrix.(row).(col + 1)
        and v4 = matrix.(row + 1).(col + 1) in
        match (v1, v2, v3, v4) with
        | `One, `One, `One, `One | `Zero, `Zero, `Zero, `Zero -> calculate_block (succ row) (succ col) (accum + score)
        | _ -> calculate_block row (succ col) accum
    in
    calculate_block 0 0 0

  let for_indicator_pattern matrix =
    let size = Array.length matrix in

    let rec check_blight_patterns pos move_fun check_border repeated_count =
      if not @@ check_border pos then repeated_count >= 4
      else
        let row, col = pos in
        match matrix.(row).(col) with
        | `Zero -> check_blight_patterns (move_fun pos) move_fun check_border (succ repeated_count)
        | `One -> check_blight_patterns (move_fun pos) move_fun check_border 0
    in

    let scoring_for_row (row, col) dir =
      match dir with
      | `After ->
          let start = (row, col + 7) in
          let move_fun (row, col) = (row, succ col) and check_border (_, v) = v >= size in
          check_blight_patterns start move_fun check_border 0
      | `Before ->
          let start = (row, col - 1) in
          let move_fun (row, col) = (row, pred col) and check_border (_, v) = v <= 0 in
          check_blight_patterns start move_fun check_border 0
    in
    let scoring_for_col (row, col) dir =
      match dir with
      | `After ->
          let start = (row + 7, col) in
          let move_fun (row, col) = (succ row, col) and check_border (v, _) = v >= size in
          check_blight_patterns start move_fun check_border 0
      | `Before ->
          let start = (row - 1, col) in
          let move_fun (row, col) = (pred row, col) and check_border (v, _) = v <= 0 in
          check_blight_patterns start move_fun check_border 0
    in
    let start_positions =
      [
        (* top-left indicator *)
        ((0, 0), `After, scoring_for_row);
        ((1, 0), `After, scoring_for_row);
        ((2, 0), `After, scoring_for_row);
        ((3, 0), `After, scoring_for_row);
        ((4, 0), `After, scoring_for_row);
        ((5, 0), `After, scoring_for_row);
        ((6, 0), `After, scoring_for_row);
        ((0, 0), `After, scoring_for_col);
        ((0, 1), `After, scoring_for_col);
        ((0, 2), `After, scoring_for_col);
        ((0, 3), `After, scoring_for_col);
        ((0, 4), `After, scoring_for_col);
        ((0, 5), `After, scoring_for_col);
        ((0, 6), `After, scoring_for_col);
        (* top-right indicator *)
        ((0, size - 7), `Before, scoring_for_row);
        ((1, size - 7), `Before, scoring_for_row);
        ((2, size - 7), `Before, scoring_for_row);
        ((3, size - 7), `Before, scoring_for_row);
        ((4, size - 7), `Before, scoring_for_row);
        ((5, size - 7), `Before, scoring_for_row);
        ((6, size - 7), `Before, scoring_for_row);
        ((0, size - 7), `After, scoring_for_col);
        ((0, size - 6), `After, scoring_for_col);
        ((0, size - 5), `After, scoring_for_col);
        ((0, size - 4), `After, scoring_for_col);
        ((0, size - 3), `After, scoring_for_col);
        ((0, size - 2), `After, scoring_for_col);
        ((0, size - 1), `After, scoring_for_col);
        (* bottom-left indicator *)
        ((size - 7, 0), `After, scoring_for_row);
        ((size - 6, 0), `After, scoring_for_row);
        ((size - 5, 0), `After, scoring_for_row);
        ((size - 4, 0), `After, scoring_for_row);
        ((size - 3, 0), `After, scoring_for_row);
        ((size - 2, 0), `After, scoring_for_row);
        ((size - 1, 0), `After, scoring_for_row);
        ((size - 7, 0), `Before, scoring_for_col);
        ((size - 6, 0), `Before, scoring_for_col);
        ((size - 5, 0), `Before, scoring_for_col);
        ((size - 4, 0), `Before, scoring_for_col);
        ((size - 3, 0), `Before, scoring_for_col);
        ((size - 2, 0), `Before, scoring_for_col);
        ((size - 1, 0), `Before, scoring_for_col);
      ]
    in
    let exists_invalid_pattern = start_positions |> List.exists (fun (start, dir, f) -> f start dir) in
    if exists_invalid_pattern then 40 else 0

  let for_dark_pattern_rate matrix =
    let total_size = Array.length matrix * 2 in

    let count = ref 0 in
    Array.iter (fun ary -> Array.iter (function `One -> incr count | `Zero -> ()) ary) matrix;

    let total_size = float_of_int total_size and count = float_of_int !count in
    let dark_module_rate = count /. total_size in
    let diff = abs_float (dark_module_rate -. 0.5) *. 10. /. 5. in
    int_of_float (diff *. 10.)
end

let apply_mask ~function_pattern matrix t =
  let module P = Type.Position_set in
  Array.mapi
    (fun row ary ->
      Array.mapi (fun col bit -> if P.mem (row, col) function_pattern then bit else Type.Bit.xor bit t.(row).(col)) ary)
    matrix

let calculate_conceded ~matrix ~function_module t =
  let function_pattern = Function_module.to_position_set function_module in
  let matrix' = apply_mask ~function_pattern matrix t in
  let conceded_of_adjacent = Conceded_score.for_adjacent matrix'
  and conceded_of_module_block = Conceded_score.for_module_block matrix'
  and conceded_of_indicator_pattern = Conceded_score.for_indicator_pattern matrix'
  and conceded_of_dark_pattern_rate = Conceded_score.for_dark_pattern_rate matrix' in
  let score =
    conceded_of_adjacent + conceded_of_module_block + conceded_of_indicator_pattern + conceded_of_dark_pattern_rate
  in
  (matrix', score)

let choice_applyable_mask ~version ~function_module ~matrix =
  let masks = [ T_2; T_3; T_4; T_5; T_6; T_7; T_8 ] in
  let mask_pattern = to_mask_pattern ~version T_1 in
  let masked, score = calculate_conceded ~function_module ~matrix mask_pattern in
  let masked, _, mask =
    List.fold_left
      (fun (masked, champion_score, mask) mask' ->
        let mask_pattern = to_mask_pattern ~version mask' in
        let masked', score = calculate_conceded ~function_module ~matrix mask_pattern in
        if champion_score <= score then (masked, champion_score, mask) else (masked', score, mask'))
      (masked, score, T_1) masks
  in
  (masked, mask)
