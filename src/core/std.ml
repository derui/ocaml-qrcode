module Result = struct
  include Stdlib.Result

  module Let_syntax = struct
    let ( let* ) = Result.bind
  end
end
