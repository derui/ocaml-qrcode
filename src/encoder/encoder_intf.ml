open Ocaml_qrcode_core

module type ENCODER = sig
  val encode : path:string -> matrix:Module_matrix.t -> unit
  (** [encode ~path ~matrix] encode the image into [path] *)
end
