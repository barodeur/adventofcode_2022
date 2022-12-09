open Core
open Aoc2022

let () =
  Aoc2022d01.run @@ fun seq ->
  let open Sequence.Monad_infix in
  seq
  >>| List.sum
  |> Sequence.to_list
  |> List.sort ~compare:(Fun.flip Int.compare)
  |> Fun.flip List.take 3
  |> List.sum
