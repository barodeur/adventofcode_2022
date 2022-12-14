open Core
open Aoc2022d08

let () =
  run @@ fun forest ->
  forest |> Forest.tree_seq |> Sequence.count ~f:Forest.Tree.is_visible
