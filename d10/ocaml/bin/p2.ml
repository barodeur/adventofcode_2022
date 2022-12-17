open Core

module Pixel = struct
  type t = Dark | Lit

  let to_char = function Dark -> '.' | Lit -> '#'
end

module Row = struct
  type t = Pixel.t array

  let empty = Array.create ~len:40 Pixel.Dark

  let apply_sprite row ~idx ~x =
    Array.set row idx (if Int.abs (idx - x) <= 1 then Pixel.Lit else Dark)

  let to_string (row : t) =
    row |> Array.to_list |> List.map ~f:Pixel.to_char |> String.of_char_list
end

let () =
  Aoc2022d10.run @@ fun instructions_seq ->
  print_endline "";
  instructions_seq
  |> Fun.flip Sequence.chunks_exn 40
  |> Sequence.map ~f:(fun cycles ->
         let row = Row.empty in
         cycles |> List.iteri ~f:(fun idx x -> Row.apply_sprite row ~idx ~x);
         row)
  |> Sequence.map ~f:Row.to_string
  |> Fun.flip Sequence.take 6
  |> Sequence.to_list
  |> String.concat ~sep:"\n"
