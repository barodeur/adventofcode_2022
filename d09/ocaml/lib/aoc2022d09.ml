open Core

module Direction = struct
  type t = Up | Right | Down | Left [@@deriving sexp]

  let from_char = function
    | 'U' -> Some Up
    | 'R' -> Some Right
    | 'D' -> Some Down
    | 'L' -> Some Left
    | _ -> None
end

module Move = struct
  type t = Direction.t * int [@@deriving sexp]

  let regex = Re2.create_exn {|([URDL])\s+(\d+)|}

  let from_string str =
    let open Option.Let_syntax in
    let%bind m = Re2.first_match regex str |> Or_error.ok in
    let%bind direction_str = Re2.Match.get m ~sub:(`Index 1) in
    let direction_char = String.get direction_str 0 in
    let%bind direction = Direction.from_char direction_char in
    let%bind distance_str = Re2.Match.get m ~sub:(`Index 2) in
    let distance = Int.of_string distance_str in
    Some (direction, distance)

  let to_steps_seq ((direction, distance) : t) =
    Sequence.range 0 distance |> Sequence.map ~f:(fun _ -> direction)

  let print move = move |> sexp_of_t |> Sexp.to_string |> print_endline
end

module Position = struct
  type t = int * int [@@deriving sexp]

  let compare = Poly.compare
  let equal = Poly.equal
  let init : t = (0, 0)
end

module PositionSet = Set.Make (Position)

module Knot = struct
  type t = Position.t

  let apply_step ((i, j) : t) ~direction =
    match direction with
    | Direction.Up -> (i + 1, j)
    | Direction.Right -> (i, j + 1)
    | Direction.Down -> (i - 1, j)
    | Direction.Left -> (i, j - 1)

  let follows ~head:(hi, hj) ~tail:(ti, tj) =
    if Int.abs (hi - ti) > 1 || Int.abs (hj - tj) > 1 then
      (ti + Int.compare hi ti, tj + Int.compare hj tj)
    else (ti, tj)
end

module TwoKnotsRope = struct
  type t = { h : Position.t; t : Position.t } [@@deriving sexp]

  let make ~h ~t = { h; t }
  let init = { h = (0, 0); t = (0, 0) }

  let apply_step { h; t } ~direction =
    let new_head = Knot.apply_step h ~direction in
    let new_tail = Knot.follows ~head:new_head ~tail:t in
    { h = new_head; t = new_tail }

  let apply_move rope ((direction, distance) : Move.t) =
    Sequence.range 0 distance
    |> Sequence.fold ~init:rope ~f:(fun r _ -> apply_step r ~direction)
end

let run f =
  Aoc2022.run @@ fun lines_seq ->
  lines_seq |> Sequence.filter_map ~f:Move.from_string |> f |> Int.to_string
