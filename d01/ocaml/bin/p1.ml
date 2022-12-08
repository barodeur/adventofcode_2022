open Core

let () =
  Aoc.run @@ fun seq ->
  seq
  |> Sequence.map ~f:(List.fold ~init:0 ~f:( + ))
  |> Sequence.max_elt ~compare:Int.compare
  |> Option.value_exn
