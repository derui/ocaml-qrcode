open Type

type module_ = Bit.t * position

type format_information = {
  top_left : position list;
  top_and_bottom : position list;
}

type version_information = {
  top_right : position list;
  bottom_left : position list;
}

type timing_pattern = {
  top_to_bottom : module_ list;
  left_to_right : module_ list;
}

(* positions is list of center position for alignment pattern *)
type alignment_pattern = { patterns : module_ list list }

(* each position is left-top position of a finder pattern *)
type finder_pattern = {
  top_left : module_ list;
  top_right : module_ list;
  bottom_left : module_ list;
}

type separator_pattern = { positions : position list }

type t = {
  finder_pattern_positions : finder_pattern;
  format_information_positions : format_information;
  version_information_positions : version_information option;
  alignment_pattern_positions : alignment_pattern;
  separator_pattern_positions : separator_pattern;
  timing_pattern_positions : timing_pattern;
  modules_per_edge : int;
}
(** type of function modules *)

let put_bit ~set_ref ~matrix ~pos ~bit =
  if Position_set.mem pos !set_ref then ()
  else
    let row, col = pos in
    matrix.(row).(col) <- bit;
    set_ref := Position_set.add pos !set_ref

let get_format_information_positions metadata =
  let capacity = Version.to_capacity metadata.Metadata.version in
  let edge = capacity.module_per_edge - 1 in

  let top_left =
    [
      (* top-left *)
      (8, 0);
      (8, 1);
      (8, 2);
      (8, 3);
      (8, 4);
      (8, 5);
      (8, 7);
      (8, 8);
      (7, 8);
      (5, 8);
      (4, 8);
      (3, 8);
      (2, 8);
      (1, 8);
      (0, 8);
    ]
  and top_and_bottom =
    [
      (* top-right *)
      (8, edge - 8);
      (8, edge - 7);
      (8, edge - 6);
      (8, edge - 5);
      (8, edge - 4);
      (8, edge - 3);
      (8, edge - 2);
      (8, edge - 1);
      (* bottom-left *)
      (edge - 8, 8);
      (edge - 7, 8);
      (edge - 6, 8);
      (edge - 5, 8);
      (edge - 4, 8);
      (edge - 3, 8);
      (edge - 2, 8);
      (edge - 1, 8);
    ]
  in
  { top_left; top_and_bottom }

let get_version_information_positions metadata =
  let capacity = Version.to_capacity metadata.Metadata.version in
  let edge = capacity.module_per_edge - 1 in
  if not @@ Metadata.need_version_information metadata then None
  else
    let bottom_left =
      [
        (* bottom-left *)
        (edge - 8, 0);
        (edge - 8, 1);
        (edge - 8, 2);
        (edge - 8, 3);
        (edge - 8, 4);
        (edge - 8, 5);
        (edge - 9, 0);
        (edge - 9, 1);
        (edge - 9, 2);
        (edge - 9, 3);
        (edge - 9, 4);
        (edge - 9, 5);
        (edge - 10, 0);
        (edge - 10, 1);
        (edge - 10, 2);
        (edge - 10, 3);
        (edge - 10, 4);
        (edge - 10, 5);
      ]
    and top_right =
      [
        (* top-right *)
        (0, edge - 8);
        (1, edge - 8);
        (2, edge - 8);
        (3, edge - 8);
        (4, edge - 8);
        (5, edge - 8);
        (0, edge - 9);
        (1, edge - 9);
        (2, edge - 9);
        (3, edge - 9);
        (4, edge - 9);
        (5, edge - 9);
        (0, edge - 10);
        (1, edge - 10);
        (2, edge - 10);
        (3, edge - 10);
        (4, edge - 10);
        (5, edge - 10);
      ]
    in
    Some { bottom_left; top_right }

let get_finder_pattern metadata =
  let capacity = metadata.Metadata.version |> Version.to_capacity in
  let as_bit v = if v = 1 then `One else `Zero in
  let edge = capacity.module_per_edge in
  let finder_pattern =
    [|
      Array.map as_bit [| 1; 1; 1; 1; 1; 1; 1 |];
      Array.map as_bit [| 1; 0; 0; 0; 0; 0; 1 |];
      Array.map as_bit [| 1; 0; 1; 1; 1; 0; 1 |];
      Array.map as_bit [| 1; 0; 1; 1; 1; 0; 1 |];
      Array.map as_bit [| 1; 0; 1; 1; 1; 0; 1 |];
      Array.map as_bit [| 1; 0; 0; 0; 0; 0; 1 |];
      Array.map as_bit [| 1; 1; 1; 1; 1; 1; 1 |];
    |]
  in
  let top_left =
    List.init (Array.length finder_pattern) (fun row ->
        List.init (Array.length finder_pattern) (fun col -> (finder_pattern.(row).(col), (row, col))))
    |> List.concat
  in
  let top_right =
    List.init (Array.length finder_pattern) (fun row ->
        List.init (Array.length finder_pattern) (fun col -> (finder_pattern.(row).(col), (row, col + edge - 7))))
    |> List.concat
  in
  let bottom_left =
    List.init (Array.length finder_pattern) (fun row ->
        List.init (Array.length finder_pattern) (fun col -> (finder_pattern.(row).(col), (row + edge - 7, col))))
    |> List.concat
  in

  { top_left; top_right; bottom_left }

let get_alignment_pattern_positions metadata =
  let alignment = Version.to_alignment_information metadata.Metadata.version in
  let as_bit v = if v = 1 then `One else `Zero in
  let alignment_pattern =
    [|
      Array.map as_bit [| 1; 1; 1; 1; 1 |];
      Array.map as_bit [| 1; 0; 0; 0; 1 |];
      Array.map as_bit [| 1; 0; 1; 0; 1 |];
      Array.map as_bit [| 1; 0; 0; 0; 1 |];
      Array.map as_bit [| 1; 1; 1; 1; 1 |];
    |]
  in
  let center_positions =
    List.fold_left
      (fun centers index -> List.map (fun c -> (index, c)) alignment.indices :: centers)
      [] alignment.indices
    |> List.concat
  in
  let patterns =
    center_positions
    |> List.map (fun (center_row, center_col) ->
           let base_row = center_row - 2 and base_col = center_col - 2 in
           List.init 5 (fun row ->
               List.init 5 (fun col -> (alignment_pattern.(row).(col), (base_row + row, base_col + col))))
           |> List.concat)
  in

  { patterns }

let get_separator_pattern_positions metadata =
  let capacity = Version.to_capacity metadata.Metadata.version in
  let separator_size = 8 in
  let top_left =
    List.init separator_size (fun v -> (separator_size - 1, v))
    @ List.init separator_size (fun v -> (v, separator_size - 1))
  and top_right =
    List.init separator_size (fun v -> (v, capacity.module_per_edge - 9))
    @ List.init separator_size (fun v -> (separator_size - 1, capacity.module_per_edge - 9 + v))
  and bottom_left =
    List.init separator_size (fun v -> (capacity.module_per_edge - 9, v))
    @ List.init separator_size (fun v -> (capacity.module_per_edge - 9 + v, separator_size - 1))
  in
  { positions = List.concat [ top_left; top_right; bottom_left ] }

let get_timing_pattern_positions metadata =
  let capacity = Version.to_capacity metadata.Metadata.version in
  let idx_to_bit i = match i mod 2 with 0 -> `One | _ -> `Zero in
  let count = capacity.module_per_edge - 16 in
  let start_pos = 8 and fixed_pos = 6 in
  let top_to_bottom =
    List.init count (fun idx ->
        let bit = idx_to_bit idx in
        (bit, (idx + start_pos, fixed_pos)))
  in
  let left_to_right =
    List.init count (fun idx ->
        let bit = idx_to_bit idx in
        (bit, (fixed_pos, idx + start_pos)))
  in
  { top_to_bottom; left_to_right }

let make metadata =
  let capacity = metadata.Metadata.version |> Version.to_capacity in
  {
    finder_pattern_positions = get_finder_pattern metadata;
    format_information_positions = get_format_information_positions metadata;
    version_information_positions = get_version_information_positions metadata;
    alignment_pattern_positions = get_alignment_pattern_positions metadata;
    separator_pattern_positions = get_separator_pattern_positions metadata;
    timing_pattern_positions = get_timing_pattern_positions metadata;
    modules_per_edge = capacity.module_per_edge;
  }

let to_position_set t =
  let open Std in
  let set = ref Position_set.empty in
  (* finder pattern *)
  set := List.map snd t.finder_pattern_positions.top_left |> List.to_seq |> Fun.swap Position_set.add_seq !set;
  set := List.map snd t.finder_pattern_positions.top_right |> List.to_seq |> Fun.swap Position_set.add_seq !set;
  set := List.map snd t.finder_pattern_positions.bottom_left |> List.to_seq |> Fun.swap Position_set.add_seq !set;
  (* format information *)
  set := t.format_information_positions.top_and_bottom |> List.to_seq |> Fun.swap Position_set.add_seq !set;
  set := t.format_information_positions.top_left |> List.to_seq |> Fun.swap Position_set.add_seq !set;
  (* version information *)
  (match t.version_information_positions with
  | None -> ()
  | Some version ->
      set := version.top_right |> List.to_seq |> Fun.swap Position_set.add_seq !set;
      set := version.bottom_left |> List.to_seq |> Fun.swap Position_set.add_seq !set);
  (* alignment pattern *)
  set :=
    t.alignment_pattern_positions.patterns |> List.concat |> List.map snd |> List.to_seq
    |> Fun.swap Position_set.add_seq !set;
  (* separator *)
  set := t.separator_pattern_positions.positions |> List.to_seq |> Fun.swap Position_set.add_seq !set;
  (* timing pattern *)
  set := List.map snd t.timing_pattern_positions.left_to_right |> List.to_seq |> Fun.swap Position_set.add_seq !set;
  set := List.map snd t.timing_pattern_positions.top_to_bottom |> List.to_seq |> Fun.swap Position_set.add_seq !set;

  !set

module Writer = struct
  let fill_finder_patterns ~matrix t =
    t.finder_pattern_positions.bottom_left |> List.iter (fun (bit, (row, col)) -> matrix.(row).(col) <- bit);
    t.finder_pattern_positions.top_left |> List.iter (fun (bit, (row, col)) -> matrix.(row).(col) <- bit);
    t.finder_pattern_positions.top_right |> List.iter (fun (bit, (row, col)) -> matrix.(row).(col) <- bit)

  let fill_separators ~matrix t =
    t.separator_pattern_positions.positions |> List.iter (fun (row, col) -> matrix.(row).(col) <- `Zero)

  let fill_timing_patterns ~matrix t =
    t.timing_pattern_positions.left_to_right |> List.iter (fun (bit, (row, col)) -> matrix.(row).(col) <- bit);
    t.timing_pattern_positions.top_to_bottom |> List.iter (fun (bit, (row, col)) -> matrix.(row).(col) <- bit)

  let fill_alignment_patterns ~matrix t =
    t.alignment_pattern_positions.patterns |> List.concat
    |> List.iter (fun (bit, (row, col)) -> matrix.(row).(col) <- bit)

  let fill_version_informations ~matrix t =
    match t.version_information_positions with
    | None -> ()
    | Some version ->
        version.top_right |> List.iter (fun (row, col) -> matrix.(row).(col) <- `Zero);
        version.bottom_left |> List.iter (fun (row, col) -> matrix.(row).(col) <- `Zero)

  let fill_format_informations ~matrix t =
    t.format_information_positions.top_and_bottom |> List.iter (fun (row, col) -> matrix.(row).(col) <- `Zero)
end
