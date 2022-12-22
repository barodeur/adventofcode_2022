open Core
open Aoc2022d12

let () =
  run @@ fun nodes ->
  let is_start node = Node.char node |> Char.equal 'S' in
  let start_node = nodes |> Hash_set.find ~f:is_start |> Option.value_exn in
  min_distance ~nodes ~start_node
