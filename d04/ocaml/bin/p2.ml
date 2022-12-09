open Core
open Aoc2022d04

let () =
  run
  @@ Sequence.count ~f:(fun (r1, r2) ->
         Range.contain r1 (fst r2) || Range.contain r2 (fst r1))
