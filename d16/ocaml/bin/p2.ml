open Core
open Aoc2022d16

module Array = struct
  include Core.Array

  let map_at arr ~idx ~f =
    Array.mapi arr ~f:(fun i item -> if i = idx then f item else item)
end

let () =
  Aoc2022d16.run @@ fun ~get_valve ~candidate_valve_ids ->
  let rec resolve ~current_valve_ids ~remaining_time ~opened_valve_ids
      ~last_visits ~total =
    if remaining_time = 0 then total
    else
      let opened_valve_ids_set =
        opened_valve_ids |> Array.to_list |> Set.union_list (module Valve.Id)
      in
      let total_flow_rate =
        opened_valve_ids_set
        |> Set.to_list
        |> List.map ~f:get_valve
        |> List.map ~f:Valve.flow_rate
        |> List.fold ~init:0 ~f:( + )
      in
      if Set.equal candidate_valve_ids opened_valve_ids_set then
        total + (remaining_time * total_flow_rate)
      else if
        current_valve_ids
        |> Fun.flip Array.zip_exn opened_valve_ids
        |> Fun.flip Array.zip_exn last_visits
        |> Array.exists
             ~f:(fun ((current_valve_id, opened_valve_ids), last_visit) ->
               Map.find last_visit current_valve_id
               |> Option.value_map ~default:false
                    ~f:(Set.equal opened_valve_ids))
      then total + (remaining_time * total_flow_rate)
      else
        current_valve_ids
        |> Array.to_list
        |> List.concat_mapi ~f:(fun idx current_valve_id ->
               (if
                current_valve_id |> get_valve |> Valve.flow_rate = 0
                || Set.mem opened_valve_ids_set current_valve_id
               then []
               else
                 [
                   resolve ~current_valve_ids
                     ~remaining_time:(remaining_time - 1)
                     ~total:(total + total_flow_rate)
                     ~last_visits:
                       (Array.map_at last_visits ~idx ~f:(fun last_visit ->
                            Map.set last_visit ~key:current_valve_id
                              ~data:opened_valve_ids_set))
                     ~opened_valve_ids:
                       (Array.map_at opened_valve_ids ~idx
                          ~f:(fun opened_valve_ids ->
                            Set.add opened_valve_ids current_valve_id));
                 ])
               |> List.append
                    (current_valve_id
                    |> get_valve
                    |> Valve.following_valve_ids
                    |> List.map ~f:(fun id ->
                           resolve
                             ~current_valve_ids:
                               (Array.map_at current_valve_ids ~idx ~f:(fun _ ->
                                    id))
                             ~remaining_time:(remaining_time - 1)
                             ~opened_valve_ids
                             ~last_visits:
                               (Array.map_at last_visits ~idx
                                  ~f:(fun last_visit ->
                                    Map.set last_visit ~key:current_valve_id
                                      ~data:opened_valve_ids_set))
                             ~total:(total + total_flow_rate))))
        |> List.max_elt ~compare:Int.compare
        |> Option.value_exn
  in
  resolve ~current_valve_ids:(Array.create ~len:2 "AA") ~remaining_time:26
    ~opened_valve_ids:(Array.create ~len:2 (Set.empty (module Valve.Id)))
    ~total:0
    ~last_visits:(Array.create ~len:2 (Map.empty (module Valve.Id)))
