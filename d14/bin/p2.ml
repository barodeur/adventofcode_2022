open Core
open Aoc2022d14

let () =
  run ~append_bottom_rocks:true @@ fun ~cave ~drop_sand ->
  Sequence.repeat ()
  |> Sequence.take_while ~f:(fun _ ->
         drop_sand () |> ignore;
         not (Cave.Cell.equal Sand cave.(500).(0)))
  |> Sequence.count ~f:(Fun.const true)
  |> ( + ) 1
