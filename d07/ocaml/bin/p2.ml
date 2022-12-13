open Core
open Aoc2022d07

let () =
  run @@ fun fs ->
  let total = 70_000_000 in
  let required = 30_000_000 in
  let root_size = Filesystem.size fs in
  let total_unused = total - root_size in
  let to_free = required - total_unused in
  fs
  |> Filesystem.to_seq
  |> Sequence.filter ~f:Filesystem.is_directory
  |> Sequence.map ~f:Filesystem.size
  |> Sequence.filter ~f:(fun size -> size >= to_free)
  |> Sequence.min_elt ~compare:Int.compare
  |> Option.value_exn
