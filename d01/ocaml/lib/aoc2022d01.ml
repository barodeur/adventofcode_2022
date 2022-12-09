open Core

let parse seq =
  let open Sequence.Monad_infix in
  seq
  |> Sequence.group ~break:(fun _ line -> line |> String.is_empty)
  >>| List.filter ~f:(Fun.negate String.is_empty)
  |> Sequence.filter ~f:(Fun.negate List.is_empty)
  >>| List.map ~f:Int.of_string

let run f = Aoc2022.run @@ fun seq -> seq |> parse |> f |> Int.to_string
