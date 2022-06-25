open Ocaml_qrcode_core

type options = { pixel_per_module : int  (** pixel size of module. This value must be greater than 0 *) }
(** Options for encoder. *)

module type ENCODER = sig
  val encode : path:string -> matrix:Module_matrix.t -> options:options -> unit
  (** [encode ~path ~matrix] encode the image into [path] *)
end
