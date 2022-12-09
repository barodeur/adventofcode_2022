open Core

module Sequence = struct
  include Core.Sequence

  let sum = fold ~init:0 ~f:( + )
end

module List = struct
  include Core.List

  let sum = fold ~init:0 ~f:( + )
end

let run f =
  Eio_main.run @@ fun env ->
  let open Eio in
  env
  |> Eio.Stdenv.stdin
  |> Buf_read.of_flow ~max_size:10_000_000
  |> Buf_read.lines
  |> Sequence.of_seq
  |> f
  |> print_endline
