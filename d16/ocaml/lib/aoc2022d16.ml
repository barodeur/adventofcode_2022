open Core

module Valve = struct
  module Id = struct
    module T = struct
      type t = string [@@deriving hash, compare, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  type t = { id : Id.t; flow_rate : int; following_valve_ids : Id.t list }
  [@@deriving sexp, fields]

  let make ~id ~flow_rate ~following_valve_ids =
    { id; flow_rate; following_valve_ids }

  let regex =
    Re2.create_exn
      {|^Valve (?P<valve_id>[A-Z]{2}) has flow rate=(?P<flow_rate>\d+); tunnels? leads? to valves? (?P<valves_list>((, )?[A-Z]{2})+)$|}

  let of_string str =
    print_endline str;
    let m = Re2.first_match_exn regex str in
    let valve_id = Re2.Match.get_exn m ~sub:(`Name "valve_id") in
    let flow_rate =
      Re2.Match.get_exn m ~sub:(`Name "flow_rate") |> Int.of_string
    in
    let following_valve_ids =
      Re2.Match.get_exn m ~sub:(`Name "valves_list")
      |> String.split ~on:','
      |> List.map ~f:String.strip
    in
    make ~id:valve_id ~flow_rate ~following_valve_ids
end

module Network = struct
  type t = (Valve.Id.t, Valve.t) Hashtbl.t [@@deriving sexp_of]

  let of_string_sequence seq : t =
    let table = Hashtbl.create (module Valve.Id) in
    seq
    |> Sequence.map ~f:Valve.of_string
    |> Sequence.iter ~f:(fun valve ->
           Hashtbl.add_exn table ~key:(Valve.id valve) ~data:valve);
    table

  let get_valve = Hashtbl.find_exn
  let valve_ids = Hashtbl.keys
end

let run f =
  Aoc2022.run @@ fun lines_seq ->
  let network = Network.of_string_sequence lines_seq in
  let get_valve = Network.get_valve network in
  let all_valve_ids =
    network |> Network.valve_ids |> Set.of_list (module Valve.Id)
  in
  let candidate_valve_ids =
    all_valve_ids
    |> Set.filter ~f:(fun id -> id |> get_valve |> Valve.flow_rate > 0)
  in
  f ~get_valve ~candidate_valve_ids |> Int.to_string
