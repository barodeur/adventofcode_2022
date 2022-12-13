open Core
open Aoc2022d07

let () =
  run @@ fun fs ->
  fs
  |> Filesystem.to_seq
  |> Sequence.filter ~f:Filesystem.is_directory
  |> Sequence.map ~f:Filesystem.size
  |> Sequence.filter ~f:(fun size -> size <= 100_000)
  |> Sequence.fold ~init:0 ~f:( + )
