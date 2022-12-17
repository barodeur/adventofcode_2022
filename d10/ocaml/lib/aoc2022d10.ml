open Core

module Instruction = struct
  type t = Noop | Addx of int [@@deriving sexp]

  let addx i = Addx i

  let of_string str =
    if String.equal str "noop" then Noop
    else String.lsplit2_exn str ~on:' ' |> snd |> Int.of_string |> addx

  let to_string i = i |> sexp_of_t |> Sexp.to_string
  let print i = i |> to_string |> print_endline
  let transform = function Noop -> [ 0 ] | Addx v -> [ 0; v ]
end

let run f =
  Aoc2022.run @@ fun lines_seq ->
  lines_seq
  |> Sequence.map ~f:Instruction.of_string
  |> Sequence.concat_map ~f:(fun i ->
         i |> Instruction.transform |> Sequence.of_list)
  |> Sequence.folding_map ~init:1 ~f:(fun x incr ->
         let r = x + incr in
         (r, r))
  |> Sequence.append ([ 0 ] |> Sequence.of_list)
  |> f
