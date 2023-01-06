open Core
open Aoc2022d16

module Action = struct
  type t = DoNothing | Open of Valve.Id.t | MoveTo of Valve.Id.t
  [@@deriving hash]
end

let find_useless_loop ~network ~actions ~start_valve_id =
  let open Action in
  let rec aux ~last_visits ~current_valve_id ~open_valves_count =
    match actions with
    | [] -> false
    | DoNothing -> false
    | Open valve_id :: tail -> true
    | MoveTo valve_id :: tail -> true
  in
  aux ~current_valve_id:start_valve_id
    ~last_visits:(Map.empty (module Valve.Id))
    ~open_valves_count:0

let evaluate_action_list ~network ~start_valve_id actions =
  let open Action in
  let rec aux ~remaining ~flow ~actions ~current_valve_id =
    if remaining = 0 then 0
    else
      match actions with
      | [] -> flow * remaining
      | DoNothing :: tail ->
          flow
          + aux ~remaining:(remaining - 1) ~flow ~actions:tail ~current_valve_id
      | Open valve_id :: tail ->
          let valve = Network.get_valve network valve_id in
          flow
          + aux ~remaining:(remaining - 1)
              ~flow:(flow + Valve.flow_rate valve)
              ~actions:tail ~current_valve_id
      | MoveTo valve_id :: tail ->
          flow
          + aux ~remaining:(remaining - 1) ~flow ~actions:tail
              ~current_valve_id:valve_id
  in
  aux ~remaining:30 ~flow:0 ~actions ~current_valve_id:start_valve_id

let possible_actions ~network ~opened_valve_ids ~from_valve_id =
  let open_actions =
    if Set.mem opened_valve_ids from_valve_id then []
    else [ Action.Open from_valve_id ]
  in
  let move_actions =
    Network.get_valve network from_valve_id
    |> Valve.following_valve_ids
    |> List.map ~f:(fun valve_id -> Action.MoveTo valve_id)
  in
  List.append open_actions move_actions

let () =
  Aoc2022d16.run @@ fun ~get_valve ~candidate_valve_ids ->
  let rec resolve ~current_valve_id ~remaining_time ~opened_valve_ids
      ~last_visits ~total =
    if remaining_time = 0 then total
    else
      let total_flow_rate =
        opened_valve_ids
        |> Set.to_list
        |> List.map ~f:get_valve
        |> List.map ~f:Valve.flow_rate
        |> List.fold ~init:0 ~f:( + )
      in
      if Set.equal candidate_valve_ids opened_valve_ids then
        total + (remaining_time * total_flow_rate)
      else if
        Map.find last_visits current_valve_id
        |> Option.value_map ~default:false ~f:(Set.equal opened_valve_ids)
      then total + (remaining_time * total_flow_rate)
      else
        []
        |> List.append
             (if
              current_valve_id |> get_valve |> Valve.flow_rate = 0
              || Set.mem opened_valve_ids current_valve_id
             then []
             else
               [
                 resolve ~current_valve_id ~remaining_time:(remaining_time - 1)
                   ~total:(total + total_flow_rate)
                   ~last_visits:
                     (Map.set last_visits ~key:current_valve_id
                        ~data:opened_valve_ids)
                   ~opened_valve_ids:(Set.add opened_valve_ids current_valve_id);
               ])
        |> List.append
             (current_valve_id
             |> get_valve
             |> Valve.following_valve_ids
             |> List.map ~f:(fun id ->
                    resolve ~current_valve_id:id
                      ~remaining_time:(remaining_time - 1) ~opened_valve_ids
                      ~last_visits:
                        (Map.set last_visits ~key:current_valve_id
                           ~data:opened_valve_ids)
                      ~total:(total + total_flow_rate)))
        |> List.max_elt ~compare:Int.compare
        |> Option.value_exn
  in
  resolve ~current_valve_id:"AA" ~remaining_time:30
    ~opened_valve_ids:(Set.empty (module Valve.Id))
    ~total:0
    ~last_visits:(Map.empty (module Valve.Id))
