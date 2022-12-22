open Core
open Aoc2022d12

let () =
  run @@ fun nodes ->
  let is_start node = Node.char node |> Char.equal 'a' in
  let start_nodes = nodes |> Hash_set.to_list |> List.filter ~f:is_start in
  printf "nodes count : %d\n" (List.length start_nodes);
  start_nodes
  |> Sequence.of_list
  |> Sequence.mapi ~f:(fun i start_node ->
         printf "%d\n" i;
         Out_channel.flush Out_channel.stdout;
         min_distance ~nodes ~start_node)
  |> Sequence.min_elt ~compare:Int.compare
  |> Option.value_exn
