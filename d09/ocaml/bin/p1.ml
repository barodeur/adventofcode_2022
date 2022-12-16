open Core
open Aoc2022d09

let () =
  run @@ fun moves_seq ->
  let _, tail_positions_set =
    moves_seq
    |> Sequence.concat_map ~f:Move.to_steps_seq
    |> Sequence.fold
         ~init:(TwoKnotsRope.init, PositionSet.singleton TwoKnotsRope.init.t)
         ~f:(fun (rope, position_set) direction ->
           let new_rope = TwoKnotsRope.apply_step rope ~direction in
           (new_rope, PositionSet.add position_set new_rope.t))
  in
  PositionSet.length tail_positions_set
