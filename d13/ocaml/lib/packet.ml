open Core

type t = Int of int | List of t list [@@deriving sexp]

let to_string packet = packet |> sexp_of_t |> Sexp.to_string

let rec compare a b =
  match (a, b) with
  | Int a, Int b -> Int.compare a b
  | List [], List [] -> 0
  | List _, Int _ -> compare a (List [ b ])
  | Int _, List _ -> 1 - compare b a
  | List [], List (_ :: _) -> -1
  | List (_ :: _), List [] -> 1 - compare b a
  | List (ha :: ta), List (hb :: tb) ->
      if compare ha hb = 0 then compare (List ta) (List tb) else compare ha hb

let equal a b = compare a b = 0
