open Core

let () =
  Aoc.run @@ fun seq ->
  seq
  |> Sequence.map ~f:(List.fold ~init:0 ~f:( + ))
  |> Sequence.to_list
  |> List.sort ~compare:(Fun.flip Int.compare)
  |> Fun.flip List.take 3
  |> List.fold ~init:0 ~f:( + )
