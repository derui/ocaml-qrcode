open Type

type t = {
  matrix : Type.Bit.t array array;
  function_module : Function_module.t;
}
(** type of matrix *)

module Support = struct
  let put_bit ~set_ref ~matrix ~pos ~bit =
    if Position_set.mem pos !set_ref then ()
    else
      let row, col = pos in
      matrix.(row).(col) <- bit;
      set_ref := Position_set.add pos !set_ref
end

module Writer = struct
  let collect_data_position_sequence ~metadata ~written_positions =
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
        let right_pos = Position_set.mem (row, base_col) written_positions
        and left_pos = Position_set.mem (row, base_col - 1) written_positions in
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

  let make_matrix ~metadata ~function_module ~blocks =
    let capacity = Version.to_capacity metadata.Metadata.version in
    let edge = capacity.module_per_edge in
    let matrix : Type.Bit.t array array = Array.make_matrix edge edge `Zero in
    let () =
      let module F = Function_module.Writer in
      F.fill_finder_patterns ~matrix function_module;
      F.fill_separators ~matrix function_module;
      F.fill_timing_patterns ~matrix function_module;
      F.fill_alignment_patterns ~matrix function_module;
      F.fill_version_informations ~matrix function_module;
      F.fill_format_informations ~matrix function_module
    in
    let written_positions = Function_module.to_position_set function_module in
    let data_module_positions = collect_data_position_sequence ~metadata ~written_positions in
    write_blocks ~blocks ~matrix ~module_positions:data_module_positions;
    { matrix; function_module }

  let write_format_information ~format_information ~metadata t =
    let capacity = Version.to_capacity metadata.Metadata.version in
    let edge = capacity.module_per_edge in
    let bits = Format_information.encode format_information |> Bit_stream.to_list in
    let matrix = Array.copy (Array.map Array.copy t.matrix) in
    let positions = t.function_module.format_information_positions in
    List.iter2 (fun (row, col) bit -> matrix.(row).(col) <- bit) positions.top_left bits;

    let other_positions = List.filter (fun (row, col) -> not (row = edge - 7 && col = 8)) positions.top_and_bottom in
    matrix.(edge - 7).(8) <- `One;

    List.iter2 (fun (row, col) bit -> matrix.(row).(col) <- bit) other_positions bits;

    { t with matrix }

  let write_version_information ~metadata t =
    let positions = t.function_module.version_information_positions in
    match (Version_information.encode metadata.Metadata.version, positions) with
    | None, _ | _, None -> t
    | Some stream, Some positions ->
        let bits = stream |> Bit_stream.to_list in
        let matrix = Array.copy (Array.map Array.copy t.matrix) in
        List.iter2 (fun (row, col) bit -> matrix.(row).(col) <- bit) positions.bottom_left bits;
        List.iter2 (fun (row, col) bit -> matrix.(row).(col) <- bit) positions.top_right bits;

        { t with matrix }
end
