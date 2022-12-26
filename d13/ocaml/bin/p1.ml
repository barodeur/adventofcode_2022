open Core
open Aoc2022d13

let () =
  run @@ fun packet_pairs_seq ->
  packet_pairs_seq
  |> Sequence.filter_mapi ~f:(fun i (a, b) ->
         if Packet.compare a b = -1 then Some (i + 1) else None)
  |> Sequence.fold ~init:0 ~f:( + )
