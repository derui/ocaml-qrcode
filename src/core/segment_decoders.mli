(** This module provides decoding facility for number mode *)

module Number : sig
  include Segment.Dec
end

module Alphabet : sig
  include Segment.Dec
end

module Byte : sig
  include Segment.Dec
end
