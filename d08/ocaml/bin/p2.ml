open Core
open Aoc2022d08

let () =
  run @@ fun forest ->
  forest
  |> Forest.tree_seq
  |> Sequence.map ~f:Forest.Tree.scenic_score
  |> Sequence.max_elt ~compare:Int.compare
  |> Option.value_exn
