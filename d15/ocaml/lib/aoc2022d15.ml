open Core

module Pos = struct
  type t = int * int [@@deriving compare, hash, sexp]

  let make x y : t = (x, y)

  let distance ((x1, y1) : t) ((x2, y2) : t) =
    Int.abs (x2 - x1) + Int.abs (y2 - y1)
end

module Line = struct
  type t = { sensor_pos : Pos.t; bacon_pos : Pos.t } [@@deriving fields]

  let int_regex = Re2.create_exn {|(-?\d+)|}

  let of_string str =
    let ints =
      Re2.get_matches_exn ~max:4 int_regex str
      |> List.map ~f:(Re2.Match.get_exn ~sub:(`Index 1))
      |> List.map ~f:Int.of_string
      |> Array.of_list
    in
    let sensor_pos = Pos.make ints.(0) ints.(1) in
    let bacon_pos = Pos.make ints.(2) ints.(3) in
    { sensor_pos; bacon_pos }
end

let run f =
  Aoc2022.run @@ fun lines_seq ->
  let table = Hashtbl.create (module Pos) in
  lines_seq
  |> Sequence.map ~f:Line.of_string
  |> Sequence.iter ~f:(fun line ->
         table
         |> Hashtbl.set ~key:(line |> Line.sensor_pos)
              ~data:(line |> Line.bacon_pos));
  f ~table |> Int.to_string
