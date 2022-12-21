open Core

module Monkey = struct
  type test = { divisor : int; true_monkey : int; false_monkey : int }
  [@@deriving sexp]

  type t = { items : int list ref; operation : int -> int; test : test }
  [@@deriving sexp]

  let parse_exp x =
    x |> String.strip |> function
    | "old" -> Fun.id
    | v -> Fun.const (Int.of_string v)

  let of_string_list str_l =
    let parts = str_l |> List.to_array in
    let items =
      Array.get parts 1
      |> String.chop_prefix_exn ~prefix:"Starting items:"
      |> String.split ~on:','
      |> List.map ~f:String.strip
      |> List.map ~f:Int.of_string
    in
    let operation_exp =
      Array.get parts 2 |> String.chop_prefix_exn ~prefix:"Operation: new = "
    in
    let operation =
      Option.merge
        ~f:(fun a _ -> a)
        (operation_exp
        |> String.lsplit2 ~on:'+'
        |> Option.map ~f:(fun (exp1, exp2) x ->
               (parse_exp exp1) x + (parse_exp exp2) x))
        (operation_exp
        |> String.lsplit2 ~on:'*'
        |> Option.map ~f:(fun (exp1, exp2) x ->
               (parse_exp exp1) x * (parse_exp exp2) x))
      |> Option.value_exn
    in
    let divisor =
      Array.get parts 3
      |> String.chop_prefix_exn ~prefix:"Test: divisible by"
      |> String.strip
      |> Int.of_string
    in
    let true_monkey =
      Array.get parts 4
      |> String.chop_prefix_exn ~prefix:"If true: throw to monkey"
      |> String.strip
      |> Int.of_string
    in
    let false_monkey =
      Array.get parts 5
      |> String.chop_prefix_exn ~prefix:"If false: throw to monkey"
      |> String.strip
      |> Int.of_string
    in
    {
      items = ref items;
      operation;
      test = { divisor; true_monkey; false_monkey };
    }

  let to_string m = m |> sexp_of_t |> Sexp.to_string
end

let run f =
  Aoc2022.run @@ fun lines_seq ->
  lines_seq
  |> Sequence.map ~f:String.strip
  |> Sequence.group ~break:(fun l1 _ -> l1 |> String.is_empty)
  |> Sequence.map ~f:Monkey.of_string_list
  |> Sequence.to_array
  |> f
  |> Array.sorted_copy ~compare:(Fun.flip Int.compare)
  |> Array.to_sequence_mutable
  |> Fun.flip Sequence.take 2
  |> Sequence.reduce ~f:( * )
  |> Option.value_exn
  |> Int.to_string
