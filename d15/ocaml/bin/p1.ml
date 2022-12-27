open Core
open Aoc2022d15

let () =
  let y = (Sys.get_argv ()).(1) |> Int.of_string in
  Aoc2022d15.run @@ fun ~table ->
  let pos_set = Hash_set.create (module Pos) in
  Hashtbl.iteri table ~f:(fun ~key:sensor_pos ~data:bacon_pos ->
      let dist = Pos.distance sensor_pos bacon_pos in
      let sensor_y = snd sensor_pos in
      let v = dist - Int.abs (sensor_y - y) in
      if v >= 0 then
        Sequence.range
          (fst sensor_pos - v)
          (fst sensor_pos + v)
          ~stop:`inclusive
        |> Sequence.map ~f:(Fun.flip Pos.make y)
        |> Sequence.iter ~f:(Hash_set.add pos_set)
      else ());
  Hashtbl.iter table ~f:(Hash_set.remove pos_set);
  pos_set |> Hash_set.length
