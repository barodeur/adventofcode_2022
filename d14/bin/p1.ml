open Core
open Aoc2022d14

let () =
  run @@ fun ~cave:_ ~drop_sand ->
  Sequence.repeat ()
  |> Sequence.take_while ~f:(fun _ -> drop_sand ())
  |> Sequence.count ~f:(Fun.const true)
