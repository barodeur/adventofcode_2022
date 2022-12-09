open Core
open Aoc2022

module Sign = struct
  type t = Rock | Paper | Scissors

  let all = [ Rock; Paper; Scissors ]

  let from_char = function
    | 'A' | 'X' -> Some Rock
    | 'B' | 'Y' -> Some Paper
    | 'C' | 'Z' -> Some Scissors
    | _ -> None

  let to_string = function
    | Rock -> "rock"
    | Paper -> "paper"
    | Scissors -> "scissors"

  let compare s1 s2 =
    match (s1, s2) with
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> 0
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> 1
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> -1

  let score = function Rock -> 1 | Paper -> 2 | Scissors -> 3
end

module Outcome = struct
  type t = Win | Lose | Draw

  let from_int i = if i < 0 then Lose else if i = 0 then Draw else Win
  let score = function Lose -> 0 | Draw -> 3 | Win -> 6
  let equal = Poly.equal
end

module Play = struct
  type t = Sign.t * Sign.t

  let make (s1 : Sign.t) (s2 : Sign.t) : t = (s1, s2)

  let from_char_pair (c1, c2) =
    match (Sign.from_char c1, Sign.from_char c2) with
    | Some s1, Some s2 -> Some (make s1 s2)
    | _ -> None

  let to_string (s1, s2) = Sign.to_string s1 ^ " " ^ Sign.to_string s2
  let to_outcome (s1, s2) : Outcome.t = Sign.compare s2 s1 |> Outcome.from_int

  let to_score (p : t) : int =
    let outcome_score = p |> to_outcome |> Outcome.score in
    let sign_score = snd p |> Sign.score in
    sign_score + outcome_score
end

let parse_line line =
  let char_at = String.get line in
  (char_at 0, char_at 2)

let run f =
  Aoc2022.run @@ fun lines_seq ->
  let open Sequence.Monad_infix in
  lines_seq >>| parse_line |> f |> Sequence.sum |> Int.to_string
