open Core
open Aoc2022d12

module Node = struct
  module T = struct
    type pos = int * int [@@deriving sexp]
    type grid = char array array [@@deriving sexp]
    type t = grid * pos [@@deriving sexp]

    let compare (_, pos_a) (_, pos_b) =
      Tuple.T2.compare ~cmp1:Int.compare ~cmp2:Int.compare pos_a pos_b

    let hash (_, (x, y)) = Int.hash x + Int.hash y
  end

  include T
  include Comparable.Make (T)

  let char (grid, (x, y)) = grid.(x).(y)
  let is_start n = n |> char |> Char.equal 'S'
  let is_end n = n |> char |> Char.equal 'E'

  let neighbors (grid, (x, y)) =
    let open Int in
    [ (-1, 0); (0, 1); (1, 0); (0, -1) ]
    |> List.map ~f:(fun (dx, dy) -> (x + dx, y + dy))
    |> List.filter ~f:(fun (nx, ny) ->
           nx >= 0
           && ny >= 0
           && nx < Array.length grid
           && ny < Array.length grid.(0))
    |> List.map ~f:(fun pos -> (grid, pos))
end

let () =
  run @@ fun lines_seq ->
  let grid =
    lines_seq |> Sequence.map ~f:String.to_array |> Sequence.to_array
  in
  let nodes =
    grid
    |> Array.concat_mapi ~f:(fun x row ->
           row |> Array.mapi ~f:(fun y _ -> (grid, (x, y))))
    |> Array.to_list
    |> Hash_set.of_list (module Node)
  in
  let start_node =
    nodes |> Hash_set.find ~f:Node.is_start |> Option.value_exn
  in
  let end_node = nodes |> Hash_set.find ~f:Node.is_end |> Option.value_exn in
  let unvisited = Hash_set.copy nodes in
  let node_distances =
    nodes
    |> Hash_set.to_list
    |> List.map ~f:(fun n ->
           let dist = if Node.equal n start_node then 0 else Int.max_value in
           (n, dist))
    |> Hashtbl.of_alist_exn (module Node)
  in
  Hash_set.iter nodes ~f:(fun node ->
      node_distances |> Hashtbl.set ~key:node ~data:Int.max_value);
  Hashtbl.set node_distances ~key:start_node ~data:0;
  while Fun.negate Hash_set.is_empty unvisited do
    let min_node =
      unvisited
      |> Hash_set.min_elt ~compare:(fun a b ->
             let dist_a = Hashtbl.find_exn node_distances a in
             let dist_b = Hashtbl.find_exn node_distances b in
             Int.compare dist_a dist_b)
      |> Option.value_exn
    in
    let min_dist = Hashtbl.find_exn node_distances min_node in
    Hash_set.remove unvisited min_node;
    min_node
    |> Node.neighbors
    |> List.iter ~f:(fun node ->
           node_distances |> Hashtbl.set ~key:node ~data:(min_dist + ))
  done;
  node_distances |> Fun.flip Hashtbl.find_exn end_node |> Int.to_string
