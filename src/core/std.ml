module Result = struct
  include Stdlib.Result

  module Let_syntax = struct
    let ( let* ) = Result.bind
  end
end

module Array = struct
  include Stdlib.Array

  let loop ?(mode = `Exclusive) ~range ~f t =
    let from, to_ = range in
    let from, to_ = if mode = `Exclusive then (from, pred to_) else (from, to_) in

    let rec loop' current =
      if current > to_ then ()
      else (
        f current t;
        loop' (succ current))
    in
    loop' from
end

module Fun = struct
  let swap f a b = f b a
end
