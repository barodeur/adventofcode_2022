open Core
open Aoc2022

let () =
  Aoc2022d01.run @@ fun seq ->
  let open Sequence.Monad_infix in
  seq >>| List.sum |> Sequence.max_elt ~compare:Int.compare |> Option.value_exn
