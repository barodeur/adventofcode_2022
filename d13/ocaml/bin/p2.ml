open Core
open Aoc2022d13

let separators = [ "[[2]]"; "[[6]]" ] |> List.filter_map ~f:Packet.of_string

let () =
  run @@ fun packet_pairs_seq ->
  packet_pairs_seq
  |> Sequence.concat_map ~f:(fun (a, b) -> [ a; b ] |> Sequence.of_list)
  |> Sequence.to_list
  |> List.append separators
  |> List.sort ~compare:Packet.compare
  |> List.filter_mapi ~f:(fun i packet ->
         if List.exists separators ~f:(Packet.equal packet) then Some (i + 1)
         else None)
  |> List.fold ~init:1 ~f:( * )
