open Encoder_intf

module Core : ENCODER = struct
  let encode ~path ~matrix ~options =
    let image = Module_encoder.encode_to_index8 ~matrix ~options in
    let options = [] in
    Png.save path options image
end

include Core
