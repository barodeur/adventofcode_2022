open Core

let () =
  Aoc2022d10.run @@ fun instructions_seq ->
  instructions_seq
  |> Sequence.mapi ~f:(fun idx x -> (idx + 1, x))
  |> Sequence.filter ~f:(fun (cycle, _) -> (cycle - 20) % 40 = 0)
  |> Sequence.fold ~init:0 ~f:(fun sum (cycle, x) -> sum + (cycle * x))
  |> Int.to_string
