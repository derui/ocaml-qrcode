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

          Rgb24.blit use_module 0 0 image x y module_pixel module_pixel)
        row)
    matrix.matrix;

  let size_with_quiet_zone = size + (4 * module_pixel * 2) in
  let image_with_quiet = Rgb24.make size_with_quiet_zone size_with_quiet_zone Constants.light_color in
  Rgb24.blit image 0 0 image_with_quiet (4 * module_pixel) (4 * module_pixel) size size;

  Images.Rgb24 image_with_quiet
