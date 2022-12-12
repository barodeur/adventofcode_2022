open Core
open Aoc2022d05

module Crane = struct
  let apply_move s m =
    let in_crane, remaining =
      Array.get s (Move.from m) |> (Fun.flip List.split_n) (Move.amount m)
    in
    Array.set s (Move.from m) remaining;
    Array.set s (Move.to_ m)
      (List.rev_append in_crane (Array.get s (Move.to_ m)))
end

let () =
  run @@ fun seq ->
  let state_list, rest_seq =
    seq |> Sequence.collect_while ~break:String.is_empty
  in
  let storage = Storage.from_string_list state_list in

  rest_seq
  |> Sequence.map ~f:Move.from_string_exn
  |> Sequence.iter ~f:(Crane.apply_move storage);

  storage |> Storage.top_boxes
