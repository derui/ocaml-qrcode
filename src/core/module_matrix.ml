type row = int

type col = int

type t = Type.Bit.t array array
(** type of matrix *)

module Position_set = Set.Make (struct
  type t = row * col

  let compare = Stdlib.compare
end)

module Support = struct
  let put_bit ~set_ref ~matrix ~pos ~bit =
    if Position_set.mem pos !set_ref then ()
    else
      let row, col = pos in
      matrix.(row).(col) <- bit;
      set_ref := Position_set.add pos !set_ref
end

module Common = struct
  let get_format_information_positions metadata =
    let capacity = Version.to_capacity metadata.Metadata.version in
    let edge = capacity.module_per_edge - 1 in

    [
      (* top-left *)
      (0, 8);
      (1, 8);
      (2, 8);
      (3, 8);
      (4, 8);
      (5, 8);
      (7, 8);
      (8, 8);
      (8, 0);
      (8, 1);
      (8, 2);
      (8, 3);
      (8, 4);
      (8, 5);
      (8, 7);
      (* top-right *)
      (8, edge - 7);
      (8, edge - 6);
      (8, edge - 4);
      (8, edge - 3);
      (8, edge - 2);
      (8, edge - 1);
      (8, edge);
      (* bottom-left *)
      (edge - 7, 8);
      (edge - 6, 8);
      (edge - 5, 8);
      (edge - 4, 8);
      (edge - 3, 8);
      (edge - 2, 8);
      (edge - 1, 8);
      (edge, 8);
    ]

  let get_version_information_positions metadata =
    let capacity = Version.to_capacity metadata.Metadata.version in
    let edge = capacity.module_per_edge - 1 in
    if not @@ Metadata.need_version_information metadata then ([], [])
    else
      ( [
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
        ],
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
        ] )
end

module Writer = struct
  let fill_finder_patterns ~metadata t =
    let capacity = Version.to_capacity metadata.Metadata.version in
    let set = ref Position_set.empty in
    let finder_pattern =
      [|
        Array.map (fun v -> if v = 1 then `One else `Zero) [| 1; 1; 1; 1; 1; 1; 1 |];
        Array.map (fun v -> if v = 1 then `One else `Zero) [| 1; 0; 0; 0; 0; 0; 1 |];
        Array.map (fun v -> if v = 1 then `One else `Zero) [| 1; 0; 1; 1; 1; 0; 1 |];
        Array.map (fun v -> if v = 1 then `One else `Zero) [| 1; 0; 1; 1; 1; 0; 1 |];
        Array.map (fun v -> if v = 1 then `One else `Zero) [| 1; 0; 1; 1; 1; 0; 1 |];
        Array.map (fun v -> if v = 1 then `One else `Zero) [| 1; 0; 0; 0; 0; 0; 1 |];
        Array.map (fun v -> if v = 1 then `One else `Zero) [| 1; 1; 1; 1; 1; 1; 1 |];
      |]
    in

    let fill_finder_pattern start_row start_col t =
      let open Std in
      Array.loop
        ~range:(start_row, start_row + Array.length finder_pattern)
        t
        ~f:(fun idx t ->
          let row = t.(idx) in
          Array.blit finder_pattern.(idx) 0 row start_col 7;
          set :=
            Position_set.add_seq
              (Array.to_list finder_pattern |> List.mapi (fun i _ -> (idx, start_col + i)) |> List.to_seq)
              !set)
    in
    let positions = [ (0, 0); (capacity.module_per_edge - 8, 0); (0, capacity.module_per_edge - 8) ] in
    List.iter (fun (row, col) -> fill_finder_pattern row col t) positions;
    (t, !set)

  let fill_separators ~metadata (t, filled_sets) =
    let capacity = Version.to_capacity metadata.Metadata.version in
    let set = ref filled_sets in
    let separator_size = 8 in

    let fill_separator_row start_row start_col t =
      let open Std in
      Array.loop
        ~range:(start_col, start_col + separator_size)
        t
        ~f:(fun idx _ ->
          let pos = (start_row, idx) in
          Support.put_bit ~set_ref:set ~matrix:t ~bit:`Zero ~pos)
    in
    let fill_separator_col start_row start_col t =
      let open Std in
      Array.loop
        ~range:(start_row, start_row + separator_size)
        t
        ~f:(fun idx _ ->
          let pos = (idx, start_col) in
          Support.put_bit ~set_ref:set ~matrix:t ~bit:`Zero ~pos)
    in
    let row_positions = [ (8, 0); (capacity.module_per_edge - 9, 0); (8, capacity.module_per_edge - 8) ] in
    let col_positions =
      [ (0, 8); (capacity.module_per_edge - 9, capacity.module_per_edge - 9); (0, capacity.module_per_edge - 8) ]
    in
    List.iter (fun (row, col) -> fill_separator_row row col t) row_positions;
    List.iter (fun (row, col) -> fill_separator_col row col t) col_positions;
    (t, !set)

  let fill_timing_patterns ~metadata (t, filled_sets) =
    let capacity = Version.to_capacity metadata.Metadata.version in
    let idx_to_bit i = match i mod 2 with 0 -> `One | _ -> `Zero in
    let set = ref filled_sets in
    let timing_pattern_col = 8 and timing_pattern_row = 8 in

    let fill_to_cols t =
      let open Std in
      Array.loop ~range:(0, capacity.module_per_edge) t ~f:(fun idx _ ->
          let pos = (idx, timing_pattern_col) in
          let bit = idx_to_bit idx in
          Support.put_bit ~set_ref:set ~matrix:t ~bit ~pos)
    in

    let fill_to_rows t =
      let open Std in
      Array.loop ~range:(0, capacity.module_per_edge) t ~f:(fun idx _ ->
          let pos = (timing_pattern_row, idx) in
          let bit = idx_to_bit idx in
          Support.put_bit ~set_ref:set ~matrix:t ~bit ~pos)
    in
    fill_to_cols t;
    fill_to_rows t;
    (t, !set)

  let fill_alignment_patterns ~metadata (t, filled_sets) =
    let alignment = Version.to_alignment_information metadata.Metadata.version in
    let set = ref filled_sets in
    let alignment_pattern =
      [|
        Array.map (fun v -> if v = 1 then `One else `Zero) [| 1; 1; 1; 1; 1 |];
        Array.map (fun v -> if v = 1 then `One else `Zero) [| 1; 0; 0; 0; 1 |];
        Array.map (fun v -> if v = 1 then `One else `Zero) [| 1; 0; 1; 0; 1 |];
        Array.map (fun v -> if v = 1 then `One else `Zero) [| 1; 0; 0; 0; 1 |];
        Array.map (fun v -> if v = 1 then `One else `Zero) [| 1; 1; 1; 1; 1 |];
      |]
    in
    let center_positions =
      List.fold_left
        (fun centers index -> List.map (fun c -> (index, c)) alignment.indices :: centers)
        [] alignment.indices
      |> List.concat
    in
    let center_positions = Position_set.add_seq (List.to_seq center_positions) Position_set.empty in

    let fill_alignment_pattern pos t =
      let open Std in
      let center_row, center_col = pos in
      if Position_set.mem pos !set then ()
      else
        Array.loop ~mode:`Inclusive
          ~range:(center_row - 2, center_row + 2)
          t
          ~f:(fun idx _ ->
            Array.blit alignment_pattern.(idx - center_row - 2) 0 t.(idx) (center_col - 2) 5;
            set :=
              Position_set.add_seq
                (Array.to_list alignment_pattern |> List.mapi (fun i _ -> (idx, center_col - 2 + i)) |> List.to_seq)
                !set)
    in
    Position_set.iter (fun center_position -> fill_alignment_pattern center_position t) center_positions;
    (t, !set)

  let fill_version_informations ~metadata (t, filled_set) =
    let set = ref filled_set in
    let first_version, second_version = Common.get_version_information_positions metadata in
    List.iter (fun pos -> Support.put_bit ~set_ref:set ~matrix:t ~pos ~bit:`Zero) (first_version @ second_version);
    (t, !set)

  let fill_format_informations ~metadata (t, filled_set) =
    let set = ref filled_set in
    let positions = Common.get_format_information_positions metadata in
    List.iter (fun pos -> Support.put_bit ~set_ref:set ~matrix:t ~pos ~bit:`Zero) positions;
    (t, !set)

  let collect_data_position_sequence ~metadata ~filled_set =
    let capacity = Version.to_capacity metadata.Metadata.version in
    let edge = capacity.module_per_edge in
    let next_row_by_dir row = function `Up -> pred row | `Down -> succ row
    and reach_end_by_dir row = function `Up -> row < 0 | `Down -> row >= edge
    and flip_dir = function `Up -> (`Down, 0) | `Down -> (`Up, edge - 1) in

    let rec loop dir row base_col queue =
      if base_col <= 0 then queue
      else if reach_end_by_dir row dir then
        let dir, row = flip_dir dir in
        loop dir row (base_col - 2) queue
      else
        let right_pos = Position_set.mem (row, base_col) filled_set
        and left_pos = Position_set.mem (row, base_col - 1) filled_set in
        if right_pos then Queue.add (row, base_col) queue else ();
        if left_pos then Queue.add (row, base_col - 1) queue else ();
        loop dir (next_row_by_dir row dir) base_col queue
    in
    loop `Up (edge - 1) (edge - 1) (Queue.create ())

  let bit_at ~at byte =
    let module U = Stdint.Uint8 in
    let at = U.shift_left U.one (at - 1) in
    let bit = U.logand at byte in
    if U.compare at bit = 0 then `One else `Zero

  let reorder_codewords (encoded : Reed_solomon_coding.t) =
    let module R = Reed_solomon_coding in
    let module U = Stdint.Uint8 in
    let block_count = Array.length encoded.R.blocks in
    let biggest_block_size =
      Array.fold_left (fun size b -> max size (U.to_int b.R.Blocks.data_word_size)) 0 encoded.blocks
    in
    let biggest_ec_block_size = Array.fold_left (fun size b -> max size (Array.length b)) 0 encoded.ec_blocks in
    let open Std in
    let data_queue = Queue.create () in
    Array.loop ~range:(0, biggest_block_size)
      ~f:(fun col blocks ->
        Array.loop ~range:(0, block_count)
          ~f:(fun row _ ->
            let block = blocks.(row) in
            let block_data = block.R.Blocks.block_data in
            if Array.length block_data <= col then () else Queue.push block_data.(col) data_queue)
          ())
      encoded.blocks;

    Array.loop ~range:(0, biggest_ec_block_size)
      ~f:(fun col blocks ->
        Array.loop ~range:(0, block_count)
          ~f:(fun row _ ->
            let block = blocks.(row) in
            if Array.length block <= col then () else Queue.push block.(col) data_queue)
          ())
      encoded.ec_blocks;
    Queue.to_seq data_queue |> Array.of_seq

  let write_blocks ~blocks ~module_positions ~matrix =
    let module R = Reed_solomon_coding in
    let queue = Queue.copy module_positions in
    let write_codeword byte =
      let bits =
        [
          bit_at ~at:8 byte;
          bit_at ~at:7 byte;
          bit_at ~at:6 byte;
          bit_at ~at:5 byte;
          bit_at ~at:4 byte;
          bit_at ~at:3 byte;
          bit_at ~at:2 byte;
          bit_at ~at:1 byte;
        ]
      in
      List.iter
        (fun bit ->
          let row, col = Queue.pop queue in
          matrix.(row).(col) <- bit)
        bits
    in
    let reordered_codewords = reorder_codewords blocks in
    Array.iter write_codeword reordered_codewords;
    (* set reminder bits *)
    Queue.iter (fun (row, col) -> matrix.(row).(col) <- `Zero) queue

  let make_matrix ~metadata ~blocks =
    let capacity = Version.to_capacity metadata.Metadata.version in
    let edge = capacity.module_per_edge in
    let matrix : Type.Bit.t array array = Array.make_matrix edge edge `Zero in
    let matrix, written_positions =
      fill_finder_patterns ~metadata matrix |> fill_timing_patterns ~metadata |> fill_separators ~metadata
      |> fill_alignment_patterns ~metadata |> fill_version_informations ~metadata |> fill_format_informations ~metadata
    in
    let data_module_positions = collect_data_position_sequence ~metadata ~filled_set:written_positions in
    write_blocks ~blocks ~matrix ~module_positions:data_module_positions;
    matrix
end
