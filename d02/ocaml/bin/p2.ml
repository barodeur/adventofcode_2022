open Core
open Aoc2022d02

module Outcome = struct
  include Aoc2022d02.Outcome

  let from_char = function
    | 'X' -> Some Lose
    | 'Y' -> Some Draw
    | 'Z' -> Some Win
    | _ -> None
end

module Line = struct
  type t = Sign.t * Outcome.t

  let from_char_pair (c1, c2) =
    match (Sign.from_char c1, Outcome.from_char c2) with
    | Some s, Some o -> Some (s, o)
    | _ -> None

  let play ((s, o) : t) =
    Sign.all
    |> List.find_map ~f:(fun s2 ->
           let play = Play.make s s2 in
           if play |> Play.to_outcome |> Outcome.equal o then Some play
           else None)
end

let () =
  run @@ fun char_pair_seq ->
  let open Sequence.Monad_infix in
  char_pair_seq
  |> Sequence.filter_map ~f:Line.from_char_pair
  |> Sequence.filter_map ~f:Line.play
  >>| Play.to_score
