open Core

module Packet = struct
  include Aoc2022d13_packet_parser.Packet

  let of_string = Aoc2022d13_packet_parser.Lex_and_parse.parse_packet
end

let run f =
  Aoc2022.run @@ fun lines_seq ->
  lines_seq
  |> Sequence.group ~break:(fun _ l2 -> String.equal l2 "")
  |> Sequence.map ~f:(fun lines ->
         lines |> List.filter ~f:(Fun.negate String.is_empty))
  |> Sequence.map ~f:(List.filter_map ~f:Packet.of_string)
  |> Sequence.filter_map ~f:(function a :: b :: _ -> Some (a, b) | _ -> None)
  |> f
  |> Int.to_string
