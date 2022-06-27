module Blocks = struct
  type block = {
    total_word_size : Stdint.Uint8.t;
    data_word_size : Stdint.Uint8.t;
    block_data : Stdint.Uint8.t array;
  }

  type t = block array

  let count_loop count f =
    let rec loop accum current = if count <= current then List.rev accum else loop (f () :: accum) (succ current) in
    loop [] 0

  let split_internal ~data_word_size ~total_word_size stream () =
    let module UI = Stdint.Uint8 in
    let bit_size = data_word_size * 8 in
    let rec loop accum count =
      if count >= bit_size then accum
      else
        match Bit_stream.next stream with
        | Eos -> failwith "Invalid bit stream"
        | Continue `One ->
            let v = accum.(count / 8) in
            let bit = UI.shift_left UI.one (7 - (count mod 8)) in
            accum.(count / 8) <- UI.(logor v bit);
            loop accum (succ count)
        | Continue `Zero -> loop accum (succ count)
    in
    let buf = Array.make data_word_size UI.zero in
    let data = loop buf 0 in
    { total_word_size = UI.of_int total_word_size; data_word_size = UI.of_int data_word_size; block_data = data }

  let split ~metadata stream =
    let open Version in
    let module E = Error_correction_level in
    let stream = Bit_stream.clone stream in
    let blocks =
      match (metadata.Metadata.version, metadata.error_correction_level) with
      | V_1, E.Low -> count_loop 1 @@ split_internal ~data_word_size:19 ~total_word_size:26 stream
      | V_1, Medium -> count_loop 1 @@ split_internal ~data_word_size:16 ~total_word_size:26 stream
      | V_1, Quality -> count_loop 1 @@ split_internal ~data_word_size:13 ~total_word_size:26 stream
      | V_1, High -> count_loop 1 @@ split_internal ~data_word_size:9 ~total_word_size:26 stream
      | V_2, Low -> count_loop 1 @@ split_internal ~data_word_size:34 ~total_word_size:44 stream
      | V_2, Medium -> count_loop 1 @@ split_internal ~data_word_size:28 ~total_word_size:44 stream
      | V_2, Quality -> count_loop 1 @@ split_internal ~data_word_size:22 ~total_word_size:44 stream
      | V_2, High -> count_loop 1 @@ split_internal ~data_word_size:16 ~total_word_size:44 stream
      | V_3, Low -> count_loop 1 @@ split_internal ~data_word_size:55 ~total_word_size:70 stream
      | V_3, Medium -> count_loop 1 @@ split_internal ~data_word_size:44 ~total_word_size:70 stream
      | V_3, Quality -> count_loop 2 @@ split_internal ~data_word_size:17 ~total_word_size:35 stream
      | V_3, High -> count_loop 2 @@ split_internal ~data_word_size:13 ~total_word_size:35 stream
      | V_4, Low -> count_loop 1 @@ split_internal ~data_word_size:80 ~total_word_size:100 stream
      | V_4, Medium -> count_loop 2 @@ split_internal ~data_word_size:32 ~total_word_size:50 stream
      | V_4, Quality -> count_loop 2 @@ split_internal ~data_word_size:24 ~total_word_size:50 stream
      | V_4, High -> count_loop 4 @@ split_internal ~data_word_size:9 ~total_word_size:25 stream
      | V_5, Low -> count_loop 1 @@ split_internal ~data_word_size:108 ~total_word_size:134 stream
      | V_5, Medium -> count_loop 2 @@ split_internal ~data_word_size:43 ~total_word_size:67 stream
      | V_5, Quality ->
          let block1 = count_loop 2 @@ split_internal ~data_word_size:15 ~total_word_size:33 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:16 ~total_word_size:34 stream in
          block1 @ block2
      | V_5, High ->
          let block1 = count_loop 2 @@ split_internal ~data_word_size:11 ~total_word_size:33 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:12 ~total_word_size:34 stream in
          block1 @ block2
      | V_6, Low -> count_loop 2 @@ split_internal ~data_word_size:68 ~total_word_size:86 stream
      | V_6, Medium -> count_loop 4 @@ split_internal ~data_word_size:27 ~total_word_size:43 stream
      | V_6, Quality -> count_loop 4 @@ split_internal ~data_word_size:19 ~total_word_size:43 stream
      | V_6, High -> count_loop 4 @@ split_internal ~data_word_size:15 ~total_word_size:43 stream
      | V_7, Low -> count_loop 2 @@ split_internal ~data_word_size:78 ~total_word_size:98 stream
      | V_7, Medium -> count_loop 4 @@ split_internal ~data_word_size:31 ~total_word_size:49 stream
      | V_7, Quality ->
          let block1 = count_loop 2 @@ split_internal ~data_word_size:14 ~total_word_size:32 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:15 ~total_word_size:33 stream in
          block1 @ block2
      | V_7, High ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:13 ~total_word_size:39 stream in
          let block2 = count_loop 1 @@ split_internal ~data_word_size:14 ~total_word_size:40 stream in
          block1 @ block2
      | V_8, Low -> count_loop 2 @@ split_internal ~data_word_size:97 ~total_word_size:121 stream
      | V_8, Medium ->
          let block1 = count_loop 2 @@ split_internal ~data_word_size:38 ~total_word_size:60 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:39 ~total_word_size:61 stream in
          block1 @ block2
      | V_8, Quality ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:18 ~total_word_size:40 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:19 ~total_word_size:41 stream in
          block1 @ block2
      | V_8, High ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:14 ~total_word_size:40 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:15 ~total_word_size:41 stream in
          block1 @ block2
      | V_9, Low -> count_loop 2 @@ split_internal ~data_word_size:116 ~total_word_size:146 stream
      | V_9, Medium ->
          let block1 = count_loop 3 @@ split_internal ~data_word_size:36 ~total_word_size:58 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:37 ~total_word_size:59 stream in
          block1 @ block2
      | V_9, Quality ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:16 ~total_word_size:36 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:17 ~total_word_size:37 stream in
          block1 @ block2
      | V_9, High ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:12 ~total_word_size:36 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:13 ~total_word_size:37 stream in
          block1 @ block2
      | V_10, Low ->
          let block1 = count_loop 2 @@ split_internal ~data_word_size:68 ~total_word_size:86 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:69 ~total_word_size:87 stream in
          block1 @ block2
      | V_10, Medium ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:43 ~total_word_size:69 stream in
          let block2 = count_loop 1 @@ split_internal ~data_word_size:44 ~total_word_size:70 stream in
          block1 @ block2
      | V_10, Quality ->
          let block1 = count_loop 6 @@ split_internal ~data_word_size:19 ~total_word_size:43 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:20 ~total_word_size:44 stream in
          block1 @ block2
      | V_10, High ->
          let block1 = count_loop 6 @@ split_internal ~data_word_size:15 ~total_word_size:43 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:16 ~total_word_size:44 stream in
          block1 @ block2
      | V_11, Low -> count_loop 4 @@ split_internal ~data_word_size:81 ~total_word_size:101 stream
      | V_11, Medium ->
          let block1 = count_loop 1 @@ split_internal ~data_word_size:50 ~total_word_size:80 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:51 ~total_word_size:81 stream in
          block1 @ block2
      | V_11, Quality ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:22 ~total_word_size:50 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:23 ~total_word_size:51 stream in
          block1 @ block2
      | V_11, High ->
          let block1 = count_loop 3 @@ split_internal ~data_word_size:12 ~total_word_size:36 stream in
          let block2 = count_loop 8 @@ split_internal ~data_word_size:13 ~total_word_size:37 stream in
          block1 @ block2
      | V_12, Low ->
          let block1 = count_loop 2 @@ split_internal ~data_word_size:92 ~total_word_size:116 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:93 ~total_word_size:117 stream in
          block1 @ block2
      | V_12, Medium ->
          let block1 = count_loop 6 @@ split_internal ~data_word_size:36 ~total_word_size:58 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:37 ~total_word_size:59 stream in
          block1 @ block2
      | V_12, Quality ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:20 ~total_word_size:46 stream in
          let block2 = count_loop 6 @@ split_internal ~data_word_size:21 ~total_word_size:47 stream in
          block1 @ block2
      | V_12, High ->
          let block1 = count_loop 7 @@ split_internal ~data_word_size:14 ~total_word_size:42 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:15 ~total_word_size:43 stream in
          block1 @ block2
      | V_13, Low -> count_loop 4 @@ split_internal ~data_word_size:107 ~total_word_size:133 stream
      | V_13, Medium ->
          let block1 = count_loop 8 @@ split_internal ~data_word_size:37 ~total_word_size:59 stream in
          let block2 = count_loop 1 @@ split_internal ~data_word_size:38 ~total_word_size:60 stream in
          block1 @ block2
      | V_13, Quality ->
          let block1 = count_loop 8 @@ split_internal ~data_word_size:20 ~total_word_size:44 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:21 ~total_word_size:45 stream in
          block1 @ block2
      | V_13, High ->
          let block1 = count_loop 12 @@ split_internal ~data_word_size:11 ~total_word_size:33 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:12 ~total_word_size:34 stream in
          block1 @ block2
      | V_14, Low ->
          let block1 = count_loop 3 @@ split_internal ~data_word_size:115 ~total_word_size:145 stream in
          let block2 = count_loop 1 @@ split_internal ~data_word_size:116 ~total_word_size:146 stream in
          block1 @ block2
      | V_14, Medium ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:40 ~total_word_size:64 stream in
          let block2 = count_loop 5 @@ split_internal ~data_word_size:41 ~total_word_size:65 stream in
          block1 @ block2
      | V_14, Quality ->
          let block1 = count_loop 11 @@ split_internal ~data_word_size:16 ~total_word_size:36 stream in
          let block2 = count_loop 5 @@ split_internal ~data_word_size:17 ~total_word_size:37 stream in
          block1 @ block2
      | V_14, High ->
          let block1 = count_loop 11 @@ split_internal ~data_word_size:12 ~total_word_size:36 stream in
          let block2 = count_loop 5 @@ split_internal ~data_word_size:13 ~total_word_size:37 stream in
          block1 @ block2
      | V_15, Low ->
          let block1 = count_loop 5 @@ split_internal ~data_word_size:87 ~total_word_size:109 stream in
          let block2 = count_loop 1 @@ split_internal ~data_word_size:88 ~total_word_size:110 stream in
          block1 @ block2
      | V_15, Medium ->
          let block1 = count_loop 5 @@ split_internal ~data_word_size:41 ~total_word_size:65 stream in
          let block2 = count_loop 5 @@ split_internal ~data_word_size:42 ~total_word_size:66 stream in
          block1 @ block2
      | V_15, Quality ->
          let block1 = count_loop 5 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 7 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_15, High ->
          let block1 = count_loop 11 @@ split_internal ~data_word_size:12 ~total_word_size:36 stream in
          let block2 = count_loop 7 @@ split_internal ~data_word_size:13 ~total_word_size:37 stream in
          block1 @ block2
      | V_16, Low ->
          let block1 = count_loop 5 @@ split_internal ~data_word_size:98 ~total_word_size:122 stream in
          let block2 = count_loop 1 @@ split_internal ~data_word_size:99 ~total_word_size:123 stream in
          block1 @ block2
      | V_16, Medium ->
          let block1 = count_loop 7 @@ split_internal ~data_word_size:45 ~total_word_size:73 stream in
          let block2 = count_loop 3 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream in
          block1 @ block2
      | V_16, Quality ->
          let block1 = count_loop 15 @@ split_internal ~data_word_size:19 ~total_word_size:43 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:20 ~total_word_size:44 stream in
          block1 @ block2
      | V_16, High ->
          let block1 = count_loop 3 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 13 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_17, Low ->
          let block1 = count_loop 1 @@ split_internal ~data_word_size:107 ~total_word_size:135 stream in
          let block2 = count_loop 5 @@ split_internal ~data_word_size:108 ~total_word_size:136 stream in
          block1 @ block2
      | V_17, Medium ->
          let block1 = count_loop 10 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream in
          let block2 = count_loop 1 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          block1 @ block2
      | V_17, Quality ->
          let block1 = count_loop 1 @@ split_internal ~data_word_size:22 ~total_word_size:50 stream in
          let block2 = count_loop 15 @@ split_internal ~data_word_size:23 ~total_word_size:51 stream in
          block1 @ block2
      | V_17, High ->
          let block1 = count_loop 2 @@ split_internal ~data_word_size:14 ~total_word_size:42 stream in
          let block2 = count_loop 17 @@ split_internal ~data_word_size:15 ~total_word_size:43 stream in
          block1 @ block2
      | V_18, Low ->
          let block1 = count_loop 5 @@ split_internal ~data_word_size:120 ~total_word_size:150 stream in
          let block2 = count_loop 1 @@ split_internal ~data_word_size:121 ~total_word_size:151 stream in
          block1 @ block2
      | V_18, Medium ->
          let block1 = count_loop 9 @@ split_internal ~data_word_size:43 ~total_word_size:69 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:44 ~total_word_size:70 stream in
          block1 @ block2
      | V_18, Quality ->
          let block1 = count_loop 17 @@ split_internal ~data_word_size:22 ~total_word_size:50 stream in
          let block2 = count_loop 1 @@ split_internal ~data_word_size:23 ~total_word_size:51 stream in
          block1 @ block2
      | V_18, High ->
          let block1 = count_loop 2 @@ split_internal ~data_word_size:14 ~total_word_size:42 stream in
          let block2 = count_loop 19 @@ split_internal ~data_word_size:15 ~total_word_size:43 stream in
          block1 @ block2
      | V_19, Low ->
          let block1 = count_loop 3 @@ split_internal ~data_word_size:113 ~total_word_size:141 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:114 ~total_word_size:142 stream in
          block1 @ block2
      | V_19, Medium ->
          let block1 = count_loop 3 @@ split_internal ~data_word_size:44 ~total_word_size:70 stream in
          let block2 = count_loop 11 @@ split_internal ~data_word_size:45 ~total_word_size:71 stream in
          block1 @ block2
      | V_19, Quality ->
          let block1 = count_loop 17 @@ split_internal ~data_word_size:21 ~total_word_size:47 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:22 ~total_word_size:48 stream in
          block1 @ block2
      | V_19, High ->
          let block1 = count_loop 9 @@ split_internal ~data_word_size:13 ~total_word_size:39 stream in
          let block2 = count_loop 16 @@ split_internal ~data_word_size:14 ~total_word_size:40 stream in
          block1 @ block2
      | V_20, Low ->
          let block1 = count_loop 3 @@ split_internal ~data_word_size:107 ~total_word_size:135 stream in
          let block2 = count_loop 5 @@ split_internal ~data_word_size:108 ~total_word_size:136 stream in
          block1 @ block2
      | V_20, Medium ->
          let block1 = count_loop 3 @@ split_internal ~data_word_size:41 ~total_word_size:67 stream in
          let block2 = count_loop 13 @@ split_internal ~data_word_size:42 ~total_word_size:68 stream in
          block1 @ block2
      | V_20, Quality ->
          let block1 = count_loop 15 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 5 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_20, High ->
          let block1 = count_loop 15 @@ split_internal ~data_word_size:15 ~total_word_size:43 stream in
          let block2 = count_loop 10 @@ split_internal ~data_word_size:16 ~total_word_size:44 stream in
          block1 @ block2
      | V_21, Low ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:116 ~total_word_size:139 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:117 ~total_word_size:140 stream in
          block1 @ block2
      | V_21, Medium -> count_loop 17 @@ split_internal ~data_word_size:42 ~total_word_size:68 stream
      | V_21, Quality ->
          let block1 = count_loop 17 @@ split_internal ~data_word_size:22 ~total_word_size:50 stream in
          let block2 = count_loop 6 @@ split_internal ~data_word_size:23 ~total_word_size:51 stream in
          block1 @ block2
      | V_21, High ->
          let block1 = count_loop 19 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          let block2 = count_loop 6 @@ split_internal ~data_word_size:17 ~total_word_size:47 stream in
          block1 @ block2
      | V_22, Low ->
          let block1 = count_loop 2 @@ split_internal ~data_word_size:111 ~total_word_size:139 stream in
          let block2 = count_loop 7 @@ split_internal ~data_word_size:112 ~total_word_size:140 stream in
          block1 @ block2
      | V_22, Medium -> count_loop 17 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream
      | V_22, Quality ->
          let block1 = count_loop 7 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 16 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_22, High -> count_loop 34 @@ split_internal ~data_word_size:13 ~total_word_size:37 stream
      | V_23, Low ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:121 ~total_word_size:151 stream in
          let block2 = count_loop 5 @@ split_internal ~data_word_size:122 ~total_word_size:152 stream in
          block1 @ block2
      | V_23, Medium ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          let block2 = count_loop 14 @@ split_internal ~data_word_size:48 ~total_word_size:76 stream in
          block1 @ block2
      | V_23, Quality ->
          let block1 = count_loop 11 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 14 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_23, High ->
          let block1 = count_loop 16 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 14 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_24, Low ->
          let block1 = count_loop 6 @@ split_internal ~data_word_size:117 ~total_word_size:147 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:118 ~total_word_size:148 stream in
          block1 @ block2
      | V_24, Medium ->
          let block1 = count_loop 6 @@ split_internal ~data_word_size:45 ~total_word_size:73 stream in
          let block2 = count_loop 14 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream in
          block1 @ block2
      | V_24, Quality ->
          let block1 = count_loop 11 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 16 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_24, High ->
          let block1 = count_loop 30 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:17 ~total_word_size:47 stream in
          block1 @ block2
      | V_25, Low ->
          let block1 = count_loop 8 @@ split_internal ~data_word_size:106 ~total_word_size:132 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:107 ~total_word_size:133 stream in
          block1 @ block2
      | V_25, Medium ->
          let block1 = count_loop 8 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          let block2 = count_loop 13 @@ split_internal ~data_word_size:48 ~total_word_size:76 stream in
          block1 @ block2
      | V_25, Quality ->
          let block1 = count_loop 7 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 22 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_25, High ->
          let block1 = count_loop 22 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 13 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_26, Low ->
          let block1 = count_loop 10 @@ split_internal ~data_word_size:114 ~total_word_size:142 stream in
          let block2 = count_loop 2 @@ split_internal ~data_word_size:115 ~total_word_size:143 stream in
          block1 @ block2
      | V_26, Medium ->
          let block1 = count_loop 19 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          block1 @ block2
      | V_26, Quality ->
          let block1 = count_loop 28 @@ split_internal ~data_word_size:22 ~total_word_size:50 stream in
          let block2 = count_loop 6 @@ split_internal ~data_word_size:23 ~total_word_size:51 stream in
          block1 @ block2
      | V_26, High ->
          let block1 = count_loop 33 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:17 ~total_word_size:47 stream in
          block1 @ block2
      | V_27, Low ->
          let block1 = count_loop 8 @@ split_internal ~data_word_size:122 ~total_word_size:152 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:123 ~total_word_size:153 stream in
          block1 @ block2
      | V_27, Medium ->
          let block1 = count_loop 22 @@ split_internal ~data_word_size:45 ~total_word_size:73 stream in
          let block2 = count_loop 3 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream in
          block1 @ block2
      | V_27, Quality ->
          let block1 = count_loop 8 @@ split_internal ~data_word_size:23 ~total_word_size:53 stream in
          let block2 = count_loop 26 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          block1 @ block2
      | V_27, High ->
          let block1 = count_loop 12 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 28 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_28, Low ->
          let block1 = count_loop 3 @@ split_internal ~data_word_size:117 ~total_word_size:147 stream in
          let block2 = count_loop 10 @@ split_internal ~data_word_size:118 ~total_word_size:148 stream in
          block1 @ block2
      | V_28, Medium ->
          let block1 = count_loop 3 @@ split_internal ~data_word_size:45 ~total_word_size:73 stream in
          let block2 = count_loop 23 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream in
          block1 @ block2
      | V_28, Quality ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 31 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_28, High ->
          let block1 = count_loop 11 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 31 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_29, Low ->
          let block1 = count_loop 7 @@ split_internal ~data_word_size:116 ~total_word_size:146 stream in
          let block2 = count_loop 7 @@ split_internal ~data_word_size:117 ~total_word_size:147 stream in
          block1 @ block2
      | V_29, Medium ->
          let block1 = count_loop 21 @@ split_internal ~data_word_size:45 ~total_word_size:73 stream in
          let block2 = count_loop 7 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream in
          block1 @ block2
      | V_29, Quality ->
          let block1 = count_loop 1 @@ split_internal ~data_word_size:23 ~total_word_size:53 stream in
          let block2 = count_loop 37 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          block1 @ block2
      | V_29, High ->
          let block1 = count_loop 19 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 26 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_30, Low ->
          let block1 = count_loop 5 @@ split_internal ~data_word_size:115 ~total_word_size:145 stream in
          let block2 = count_loop 10 @@ split_internal ~data_word_size:116 ~total_word_size:146 stream in
          block1 @ block2
      | V_30, Medium ->
          let block1 = count_loop 19 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          let block2 = count_loop 10 @@ split_internal ~data_word_size:48 ~total_word_size:76 stream in
          block1 @ block2
      | V_30, Quality ->
          let block1 = count_loop 15 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 25 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_30, High ->
          let block1 = count_loop 23 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 25 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_31, Low ->
          let block1 = count_loop 13 @@ split_internal ~data_word_size:115 ~total_word_size:145 stream in
          let block2 = count_loop 3 @@ split_internal ~data_word_size:116 ~total_word_size:146 stream in
          block1 @ block2
      | V_31, Medium ->
          let block1 = count_loop 2 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream in
          let block2 = count_loop 29 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          block1 @ block2
      | V_31, Quality ->
          let block1 = count_loop 42 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 1 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_31, High ->
          let block1 = count_loop 23 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 28 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_32, Low -> count_loop 17 @@ split_internal ~data_word_size:115 ~total_word_size:145 stream
      | V_32, Medium ->
          let block1 = count_loop 10 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream in
          let block2 = count_loop 23 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          block1 @ block2
      | V_32, Quality ->
          let block1 = count_loop 10 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 35 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_32, High ->
          let block1 = count_loop 19 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 35 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_33, Low ->
          let block1 = count_loop 17 @@ split_internal ~data_word_size:115 ~total_word_size:145 stream in
          let block2 = count_loop 1 @@ split_internal ~data_word_size:116 ~total_word_size:146 stream in
          block1 @ block2
      | V_33, Medium ->
          let block1 = count_loop 14 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream in
          let block2 = count_loop 31 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          block1 @ block2
      | V_33, Quality ->
          let block1 = count_loop 29 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 19 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_33, High ->
          let block1 = count_loop 11 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 46 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_34, Low ->
          let block1 = count_loop 13 @@ split_internal ~data_word_size:115 ~total_word_size:145 stream in
          let block2 = count_loop 6 @@ split_internal ~data_word_size:116 ~total_word_size:146 stream in
          block1 @ block2
      | V_34, Medium ->
          let block1 = count_loop 14 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream in
          let block2 = count_loop 23 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          block1 @ block2
      | V_34, Quality ->
          let block1 = count_loop 44 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 7 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_34, High ->
          let block1 = count_loop 59 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          let block2 = count_loop 1 @@ split_internal ~data_word_size:17 ~total_word_size:47 stream in
          block1 @ block2
      | V_35, Low ->
          let block1 = count_loop 12 @@ split_internal ~data_word_size:121 ~total_word_size:151 stream in
          let block2 = count_loop 7 @@ split_internal ~data_word_size:122 ~total_word_size:152 stream in
          block1 @ block2
      | V_35, Medium ->
          let block1 = count_loop 12 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          let block2 = count_loop 26 @@ split_internal ~data_word_size:48 ~total_word_size:76 stream in
          block1 @ block2
      | V_35, Quality ->
          let block1 = count_loop 39 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 14 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_35, High ->
          let block1 = count_loop 22 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 41 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_36, Low ->
          let block1 = count_loop 6 @@ split_internal ~data_word_size:121 ~total_word_size:151 stream in
          let block2 = count_loop 14 @@ split_internal ~data_word_size:122 ~total_word_size:152 stream in
          block1 @ block2
      | V_36, Medium ->
          let block1 = count_loop 6 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          let block2 = count_loop 34 @@ split_internal ~data_word_size:48 ~total_word_size:76 stream in
          block1 @ block2
      | V_36, Quality ->
          let block1 = count_loop 46 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 10 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_36, High ->
          let block1 = count_loop 2 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 64 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_37, Low ->
          let block1 = count_loop 17 @@ split_internal ~data_word_size:122 ~total_word_size:152 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:123 ~total_word_size:153 stream in
          block1 @ block2
      | V_37, Medium ->
          let block1 = count_loop 29 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream in
          let block2 = count_loop 14 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          block1 @ block2
      | V_37, Quality ->
          let block1 = count_loop 49 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 10 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_37, High ->
          let block1 = count_loop 24 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 46 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_38, Low ->
          let block1 = count_loop 4 @@ split_internal ~data_word_size:122 ~total_word_size:152 stream in
          let block2 = count_loop 18 @@ split_internal ~data_word_size:123 ~total_word_size:153 stream in
          block1 @ block2
      | V_38, Medium ->
          let block1 = count_loop 13 @@ split_internal ~data_word_size:46 ~total_word_size:74 stream in
          let block2 = count_loop 32 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          block1 @ block2
      | V_38, Quality ->
          let block1 = count_loop 48 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 14 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_38, High ->
          let block1 = count_loop 42 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 32 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_39, Low ->
          let block1 = count_loop 20 @@ split_internal ~data_word_size:117 ~total_word_size:147 stream in
          let block2 = count_loop 4 @@ split_internal ~data_word_size:118 ~total_word_size:148 stream in
          block1 @ block2
      | V_39, Medium ->
          let block1 = count_loop 40 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          let block2 = count_loop 7 @@ split_internal ~data_word_size:48 ~total_word_size:76 stream in
          block1 @ block2
      | V_39, Quality ->
          let block1 = count_loop 43 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 22 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_39, High ->
          let block1 = count_loop 10 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 67 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
      | V_40, Low ->
          let block1 = count_loop 19 @@ split_internal ~data_word_size:118 ~total_word_size:148 stream in
          let block2 = count_loop 6 @@ split_internal ~data_word_size:119 ~total_word_size:149 stream in
          block1 @ block2
      | V_40, Medium ->
          let block1 = count_loop 18 @@ split_internal ~data_word_size:47 ~total_word_size:75 stream in
          let block2 = count_loop 31 @@ split_internal ~data_word_size:48 ~total_word_size:76 stream in
          block1 @ block2
      | V_40, Quality ->
          let block1 = count_loop 34 @@ split_internal ~data_word_size:24 ~total_word_size:54 stream in
          let block2 = count_loop 34 @@ split_internal ~data_word_size:25 ~total_word_size:55 stream in
          block1 @ block2
      | V_40, High ->
          let block1 = count_loop 20 @@ split_internal ~data_word_size:15 ~total_word_size:45 stream in
          let block2 = count_loop 61 @@ split_internal ~data_word_size:16 ~total_word_size:46 stream in
          block1 @ block2
    in
    Array.of_list blocks
end

type ec_block = Rs_symbol.t array

type t = {
  blocks : Blocks.t;
  ec_blocks : ec_block array;
}

(** [calculate_ec block] calculate *)
let calculate_ec ~metadata code_word =
  let stream = Code_word.to_bit_stream code_word in
  let blocks = Blocks.split ~metadata stream in
  let circuit block =
    let module U = Stdint.Uint8 in
    let k = U.(block.Blocks.total_word_size - block.data_word_size |> to_int) in
    let register = Array.make k U.zero in

    let data = block.block_data in
    let data_polynomial =
      match Rs_polynomial.to_validated k |> Option.map Rs_polynomial.from_validated with
      | None -> raise (Invalid_argument (Printf.sprintf "Invalid polynomial number: %d" k))
      | Some v -> v
    in
    let greater_index = k - 1 in
    let prev = ref U.zero in

    let put_datum datum =
      let datum = U.(logxor datum register.(greater_index)) in
      Array.iteri
        (fun idx _ ->
          let coefficient = data_polynomial.(idx) in
          let previous_data = if idx = 0 then U.zero else !prev in
          let open U in
          prev := register.(idx);
          register.(idx) <- (datum * coefficient) + previous_data)
        register
    in
    Array.iter put_datum data;
    Array.iter put_datum @@ Array.make k U.zero;

    register |> Array.to_list |> List.rev |> Array.of_list
  in
  let ec_blocks = Array.map circuit blocks in
  { blocks; ec_blocks }
