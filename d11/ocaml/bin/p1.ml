open Core
open Aoc2022d11

let () =
  run @@ fun monkeys ->
  let monkeys_count = Array.length monkeys in
  let inspection_counts = Array.create ~len:monkeys_count 0 in
  let increment_inspection_count monkey_id =
    Array.set inspection_counts monkey_id
      (Array.get inspection_counts monkey_id + 1)
  in
  Sequence.unfold ~init:1 ~f:(fun i -> Some (i, i + 1))
  |> Sequence.concat_map ~f:(fun round ->
         Sequence.range 0 monkeys_count
         |> Sequence.map ~f:(fun monkey_idx -> (round, monkey_idx)))
  |> Sequence.take_while ~f:(fun (round, _) -> round <= 20)
  |> Sequence.iter ~f:(fun (_, monkey_idx) ->
         let monkey = Array.get monkeys monkey_idx in
         let items = !(monkey.items) in
         monkey.items := [];
         List.iter items ~f:(fun wlevel ->
             increment_inspection_count monkey_idx;
             let new_wlevel = monkey.operation wlevel / 3 in
             let target_monkey_idx =
               if new_wlevel % monkey.test.divisor = 0 then
                 monkey.test.true_monkey
               else monkey.test.false_monkey
             in
             let target_monkey = Array.get monkeys target_monkey_idx in
             target_monkey.items := new_wlevel :: !(target_monkey.items)));
  inspection_counts
