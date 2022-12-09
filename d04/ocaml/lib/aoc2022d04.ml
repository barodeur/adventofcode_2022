open Core

module Range = struct
  type t = int * int

  let make i1 i2 : t = (i1, i2)
  let cover ((s1, e1) : t) ((s2, e2) : t) : bool = s1 <= s2 && e1 >= e2
  let contain ((s1, e1) : t) elt : bool = elt >= s1 && elt <= e1
end

let run f =
  Aoc2022.run @@ fun seq ->
  let open Sequence.Monad_infix in
  seq
  |> Sequence.filter_map ~f:(String.rsplit2 ~on:',')
  >>| Tuple.T2.map ~f:(String.rsplit2_exn ~on:'-')
  >>| Tuple.T2.map ~f:(Tuple.T2.map ~f:Int.of_string)
  >>| Tuple.T2.map ~f:(Tuple.T2.uncurry Range.make)
  |> f
  |> Int.to_string
