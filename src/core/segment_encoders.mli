(** This module provides encoding facility for number mode *)

module Number : sig
  include Segment.Enc
end

module Alphabet : sig
  include Segment.Enc
end

module Byte : sig
  include Segment.Enc
end
