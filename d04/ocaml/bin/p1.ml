open Core
open Aoc2022d04

let () =
  run
  @@ Sequence.count ~f:(fun (r1, r2) -> Range.cover r1 r2 || Range.cover r2 r1)
