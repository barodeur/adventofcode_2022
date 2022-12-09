open Core
open Aoc2022

module Item = struct
  type t = char

  let rec score =
    let open Char in
    function
    | c when c >= 'a' && c <= 'z' -> Char.to_int c - Char.to_int 'a' + 1
    | c when c >= 'A' && c <= 'Z' -> 26 + score (Char.lowercase c)
    | _ -> failwith "Unkown char"
end

let parse_line str = str |> String.to_array

let run f =
  Aoc2022.run @@ fun lines_seq ->
  let open Sequence.Monad_infix in
  lines_seq >>| parse_line |> f |> Sequence.sum |> Int.to_string
