open Core

let parse flow =
  let open Eio in
  let open Sequence.Monad_infix in
  flow
  |> Buf_read.of_flow ~max_size:10_000_000
  |> Buf_read.lines
  |> Sequence.of_seq
  |> Sequence.group ~break:(fun _ line -> line |> String.is_empty)
  >>| List.filter ~f:(Fun.negate String.is_empty)
  |> Sequence.filter ~f:(Fun.negate List.is_empty)
  >>| List.map ~f:Int.of_string

let run f =
  Eio_main.run @@ fun env ->
  env |> Eio.Stdenv.stdin |> parse |> f |> Int.to_string |> print_endline
