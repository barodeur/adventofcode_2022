open Core

let int_of_char c = c |> Char.to_string |> Int.of_string

type direction = Up | Right | Down | Left

let directions = [ Up; Right; Down; Left ]

let opposite_direction = function
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left

module Forest = struct
  type forest = int array array

  let row_size forest = Array.get forest 0 |> Array.length
  let col_size forest = Array.length forest

  module Tree = struct
    type t = forest * (int * int)

    let top ((forest, (i, j)) : t) : t option =
      if i = 0 then None else Some (forest, (i - 1, j))

    let right ((forest, (i, j)) : t) : t option =
      if j = row_size forest - 1 then None else Some (forest, (i, j + 1))

    let bottom ((forest, (i, j)) : t) : t option =
      if i = col_size forest - 1 then None else Some (forest, (i + 1, j))

    let left ((forest, (i, j)) : t) : t option =
      if j = 0 then None else Some (forest, (i, j - 1))

    let neighbor = function
      | Up -> top
      | Right -> right
      | Down -> bottom
      | Left -> left

    let size ((forest, (i, j)) : t) = Array.get forest i |> Fun.flip Array.get j

    let rec aligned_trees_seq tree ~direction =
      (neighbor direction) tree
      |> Option.map ~f:(fun ntree ->
             Sequence.append (Sequence.return ntree)
               (aligned_trees_seq ntree ~direction))
      |> Option.value ~default:Sequence.empty

    let is_visible_from tree ~direction =
      aligned_trees_seq tree ~direction
      |> Sequence.for_all ~f:(fun ntree -> size ntree < size tree)

    let is_visible tree =
      directions
      |> List.exists ~f:(fun direction -> is_visible_from tree ~direction)

    let viewing_distance tree ~direction =
      let tree_array = aligned_trees_seq tree ~direction |> Sequence.to_array in
      tree_array
      |> Array.find_mapi ~f:(fun i ntree ->
             if size ntree >= size tree then Some i else None)
      |> Option.map ~f:(fun d -> d + 1)
      |> Option.value ~default:(tree_array |> Array.length)

    let scenic_score tree =
      directions
      |> List.map ~f:(fun direction -> viewing_distance tree ~direction)
      |> List.reduce ~f:( * )
      |> Option.value ~default:1
  end

  let tree_seq forest : Tree.t Sequence.t =
    Sequence.range 0 (col_size forest)
    |> Sequence.map ~f:(fun i ->
           Sequence.range 0 (row_size forest)
           |> Sequence.map ~f:(fun j -> (i, j)))
    |> Sequence.concat
    |> Sequence.map ~f:(fun pos -> (forest, pos))

  let print f =
    Array.iter f ~f:(fun row ->
        Array.iter row ~f:(Printf.printf "%d");
        Printf.printf "\n")

  let from_lines_seq seq : forest =
    seq
    |> Sequence.map ~f:String.to_array
    |> Sequence.map ~f:(Array.map ~f:int_of_char)
    |> Sequence.to_array
end

let run f =
  Aoc2022.run @@ fun seq -> seq |> Forest.from_lines_seq |> f |> Int.to_string
