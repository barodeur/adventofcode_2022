open Core
open Aoc2022
open Aoc2022d02

let () =
  Aoc2022d02.run @@ fun char_pair_seq ->
  let open Sequence.Monad_infix in
  char_pair_seq |> Sequence.filter_map ~f:Play.from_char_pair >>| Play.to_score
