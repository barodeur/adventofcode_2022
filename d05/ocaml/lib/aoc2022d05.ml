open Core

let run = Aoc2022.run

module Sequence = struct
  include Core.Sequence

  let collect_while seq ~break =
    let rec loop list s =
      match Sequence.next s with
      | Some (a, nseq) when break a -> (List.rev list, nseq)
      | Some (a, nseq) -> loop (a :: list) nseq
      | None -> (List.rev list, Sequence.empty)
    in
    loop [] seq
end

module Move = struct
  type t = { amount : int; from : int; to_ : int }

  let r = Re2.create_exn {|move\s+(\d+)\s+from\s+(\d+)\s+to\s+(\d+)|}

  let from_string_exn str =
    let m = Re2.first_match_exn r str in
    let capture i = m |> Re2.Match.get_exn ~sub:(`Index i) |> Int.of_string in
    let amount = capture 1 in
    let from = capture 2 - 1 in
    let to_ = capture 3 - 1 in
    { amount; from; to_ }

  let amount = function { amount; _ } -> amount
  let from = function { from; _ } -> from
  let to_ = function { to_; _ } -> to_
end

module Storage = struct
  type t = char list array

  let from_string_list state_list =
    let col_count = ((List.nth_exn state_list 0 |> String.length) / 4) + 1 in
    let state = Array.create ~len:col_count [] in
    state_list
    |> List.iter ~f:(fun line ->
           line
           |> String.to_list
           |> List.chunks_of ~length:4
           |> List.map ~f:(fun list -> List.nth_exn list 1)
           |> List.iteri ~f:(fun i c ->
                  if Char.is_uppercase c then
                    Array.set state i (c :: Array.get state i)));
    state |> Array.map_inplace ~f:List.rev;
    state

  let top_boxes (storage : t) =
    storage
    |> Array.filter_map ~f:(Fun.flip List.nth 0)
    |> Array.map ~f:Char.to_string
    |> String.concat_array

  let pp (s : t) =
    s
    |> Array.map ~f:(fun l ->
           l |> List.map ~f:Char.to_string |> List.rev |> String.concat)
    |> Array.iteri ~f:(fun i l -> Printf.sprintf "%d %s" i l |> print_endline)
end
