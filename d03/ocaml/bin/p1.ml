open Core
open Aoc2022
open Aoc2022d03
module CharSet = Set.Make (Char)

let array_halves arr =
  arr |> Array.partitioni_tf ~f:(fun i _ -> i >= Array.length arr / 2)

let () =
  Aoc2022d03.run @@ fun seq ->
  let open Sequence.Monad_infix in
  seq
  >>| array_halves
  >>| Tuple.T2.map ~f:Array.to_sequence
  >>| Tuple.T2.map ~f:CharSet.of_sequence
  >>| Tuple.T2.uncurry CharSet.inter
  |> Sequence.filter_map ~f:CharSet.choose
  >>| Item.score
