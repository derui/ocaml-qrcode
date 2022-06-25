open Ocaml_qrcode_core
open Encoder_intf

module Constants = struct
  let light_color = { Color.r = 255; b = 255; g = 255 }

  let dark_color = { Color.r = 0; b = 0; g = 0 }
end

(** [encode_to_index8 ~matrix ~options] makes new image of index8 for matrix. *)
let encode_to_index8 ~matrix ~options =
  let edge = matrix.Module_matrix.function_module.modules_per_edge in
  let module_pixel = options.pixel_per_module in
  let size = edge * module_pixel in
  let image = Rgb24.create size size in
  let light_module = Rgb24.make module_pixel module_pixel Constants.light_color
  and dark_module = Rgb24.make module_pixel module_pixel Constants.dark_color in

  Array.iteri
    (fun rowidx row ->
      Array.iteri
        (fun colidx value ->
          let use_module = match value with `One -> dark_module | `Zero -> light_module in

          let y = rowidx * module_pixel and x = colidx * module_pixel in
          Printf.printf "(%d, %d) -> (%s)\n" rowidx colidx (Type.Bit.show value);
          Printf.printf "at image (%d, %d) -> (%s)\n" y x (Type.Bit.show value);

          Rgb24.blit use_module 0 0 image x y module_pixel module_pixel)
        row)
    matrix.matrix;

  Images.Rgb24 image
