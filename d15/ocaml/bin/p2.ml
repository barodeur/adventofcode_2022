open Core
open Aoc2022d15

let () =
  let max_dim = (Sys.get_argv ()).(1) |> Int.of_string in
  Aoc2022d15.run @@ fun ~table ->
  let is_in_sensor_range pos =
    Hashtbl.existsi table ~f:(fun ~key:sensor_pos ~data:beacon_pos ->
        Pos.distance sensor_pos pos < Pos.distance sensor_pos beacon_pos)
  in
  let is_in_range v = v >= 0 && v <= max_dim in
  Hashtbl.to_alist table
  |> Sequence.of_list
  |> Sequence.concat_map ~f:(fun (sensor_pos, beacon_pos) ->
         let dist = Pos.distance sensor_pos beacon_pos in
         Sequence.range ~stop:`inclusive 0 (dist + 1)
         |> Sequence.concat_map ~f:(fun i ->
                let sx, sy = sensor_pos in
                [
                  (sx + i, sy - (dist + 1) + i);
                  (sx + (dist + 1) - i, sy + i);
                  (sx - i, sy + (dist + 1) - i);
                  (sx - (dist + 1) + i, sy + i);
                ]
                |> Sequence.of_list))
  |> Sequence.filter ~f:(fun (x, y) -> is_in_range x && is_in_range y)
  |> Sequence.find_exn ~f:(Fun.negate is_in_sensor_range)
  |> fun (x, y) -> (x * 4000000) + y
