open Core

let range_from_points (x1, y1) (x2, y2) =
  if x1 = x2 then
    Sequence.range ~stride:(Int.compare y2 y1) ~stop:`inclusive y1 y2
    |> Sequence.map ~f:(fun y -> (x1, y))
  else
    Sequence.range ~stride:(Int.compare x2 x1) ~stop:`inclusive x1 x2
    |> Sequence.map ~f:(fun x -> (x, y1))

let rec pairs = function
  | a :: b :: rest -> (a, b) :: pairs (b :: rest)
  | _ -> []

module Cave = struct
  module Cell = struct
    type t = Empty | Rock | Sand [@@deriving equal]

    let to_char = function Empty -> '.' | Rock -> '#' | Sand -> 'o'
  end

  module Coord = struct
    type t = int * int

    let make x y = (x, y)
    let regex = Re2.create_exn {|(\d+),(\d+)|}
    let start = (500, 0)
    let ( + ) ((x1, y1) : t) ((x2, y2) : t) = (x1 + x2, y1 + y2)
    let depth ((_, y) : t) = y
  end

  module RockPath = struct
    type t = Coord.t list

    let of_string line : t option =
      let open Option in
      line
      |> Re2.get_matches Coord.regex
      |> Or_error.ok
      >>| List.map ~f:(fun m ->
              (1, 2)
              |> Tuple.T2.map ~f:(fun idx ->
                     Re2.Match.get_exn m ~sub:(`Index idx) |> Int.of_string))
  end

  type t = Cell.t array array

  let cell_at cave ((x, y) : Coord.t) = cave.(x).(y)
  let set_cell_at cave ((x, y) : Coord.t) ~value = cave.(x).(y) <- value
  let depth cave = Array.length cave.(0)

  let of_rock_path_list ~append_bottom_rocks l : t =
    let max_y =
      l
      |> List.concat_map ~f:(List.map ~f:snd)
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
    in
    let cave =
      Array.make_matrix ~dimx:1000
        ~dimy:(max_y + 1 + if append_bottom_rocks then 3 else 0)
        Cell.Empty
    in
    let _ = cave in
    l
    |> List.concat_map ~f:pairs
    |> Fun.flip List.append
         (if append_bottom_rocks then [ ((0, max_y + 2), (999, max_y + 2)) ]
         else [])
    |> Sequence.of_list
    |> Sequence.concat_map ~f:(Tuple.T2.uncurry range_from_points)
    |> Sequence.iter ~f:(set_cell_at cave ~value:Cell.Rock);
    cave

  let print cave ~min_x ~max_x =
    let max_y = Array.length cave.(0) in
    Sequence.range 0 max_y
    |> Sequence.iter ~f:(fun y ->
           Sequence.range min_x max_x
           |> Sequence.iter ~f:(fun x ->
                  let coord = Coord.make x y in
                  let cell = cell_at cave coord in
                  printf "%c" (cell |> Cell.to_char));
           printf "\n")
end

let run ?(append_bottom_rocks = false) f =
  Aoc2022.run @@ fun lines_seq ->
  let cave =
    lines_seq
    |> Sequence.filter_map ~f:Cave.RockPath.of_string
    |> Sequence.to_list
    |> Cave.of_rock_path_list ~append_bottom_rocks
  in
  let depth = Cave.depth cave in
  let cell_at = Cave.cell_at cave in
  let set_cell_at = Cave.set_cell_at cave in
  let rec sand_pos coord =
    if Cave.Coord.depth coord >= depth then Some None
    else
      match cell_at coord with
      | Rock | Sand -> None
      | Empty ->
          [ (0, 1); (-1, 1); (1, 1) ]
          |> List.find_map ~f:(fun dcoord ->
                 sand_pos (Cave.Coord.( + ) coord dcoord))
          |> Option.value_map ~default:(Some (Some coord)) ~f:(fun p -> Some p)
  in
  let drop_sand () =
    sand_pos Cave.Coord.start
    |> Option.value_exn
    |> Option.map ~f:(set_cell_at ~value:Cave.Cell.Sand)
    |> Option.is_some
  in
  f ~cave ~drop_sand |> Int.to_string
