open Core
open Aoc2022d09

let each_pair arr =
  Array.folding_map arr ~init:None ~f:(fun prev_opt item ->
      (Some item, prev_opt |> Option.map ~f:(fun prev -> (prev, item))))
  |> Array.filter_map ~f:Fun.id

module Rope = struct
  type t = Position.t array [@@deriving sexp]

  let tail = Array.last
  let init : t = Array.init 10 ~f:(Fun.const Position.init)

  let apply_step rope ~direction =
    rope
    |> Array.folding_map ~init:None ~f:(fun prev_knot_opt knot ->
           let knot =
             match prev_knot_opt with
             | Some prev_knot -> Knot.follows ~head:prev_knot ~tail:knot
             | None -> Knot.apply_step knot ~direction
           in
           (Some knot, knot))

  let print rope = rope |> sexp_of_t |> Sexp.to_string |> print_endline
end

let () =
  run @@ fun moves_seq ->
  let _, tail_positions_set =
    moves_seq
    |> Sequence.concat_map ~f:Move.to_steps_seq
    |> Sequence.fold
         ~init:(Rope.init, PositionSet.singleton (Rope.tail Rope.init))
         ~f:(fun (rope, position_set) direction ->
           let new_rope = Rope.apply_step rope ~direction in
           Rope.print new_rope;
           let new_tail = Rope.tail new_rope in
           (new_rope, PositionSet.add position_set new_tail))
  in
  PositionSet.length tail_positions_set
