open Core
open Aoc2022
open Aoc2022d03
module CharSet = Set.Make (Char)

let () =
  Aoc2022d03.run @@ fun seq ->
  let open Sequence.Monad_infix in
  seq
  >>| Array.to_sequence
  >>| CharSet.of_sequence
  |> Fun.flip Sequence.chunks_exn 3
  |> Sequence.filter_map ~f:(List.reduce ~f:CharSet.inter)
  |> Sequence.filter_map ~f:CharSet.choose
  >>| Item.score
