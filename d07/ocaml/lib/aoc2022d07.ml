open Core

module Line = struct
  module Path = struct
    type t = Root | Back | Custom of string [@@deriving sexp]

    let from_string = function "/" -> Root | ".." -> Back | str -> Custom str
  end

  module Command = struct
    type t = Cd of Path.t | Ls [@@deriving sexp]

    let cd_regex = Re2.create_exn {|cd\s+(.*)|}

    let from_string str =
      match Re2.first_match cd_regex str with
      | Ok m -> Cd (Re2.Match.get_exn ~sub:(`Index 1) m |> Path.from_string)
      | Error _ -> Ls
  end

  module File = struct
    type t = { name : string; size : int } [@@deriving sexp]

    let regex = Re2.create_exn {|(\d+)\s+([^\s]+)|}

    let from_string str =
      let m = Re2.first_match_exn regex str in
      {
        name = Re2.Match.get_exn ~sub:(`Index 2) m;
        size = Re2.Match.get_exn ~sub:(`Index 1) m |> Int.of_string;
      }
  end

  module ListItem = struct
    type t = File of File.t | Directory of string [@@deriving sexp]

    let dir_regex = Re2.create_exn {|dir\s+([^\s]+)|}

    let from_string str =
      match Re2.first_match dir_regex str with
      | Ok m -> Directory (m |> Re2.Match.get_exn ~sub:(`Index 1))
      | Error _ -> File (File.from_string str)
  end

  type t = Command of Command.t | ListItem of ListItem.t [@@deriving sexp]

  let command_regex = Re2.create_exn {|\$\s+(.*)|}

  let from_string str =
    match Re2.first_match command_regex str with
    | Ok m ->
        Command (m |> Re2.Match.get_exn ~sub:(`Index 1) |> Command.from_string)
    | Error _ -> ListItem (ListItem.from_string str)
end

module Filesystem = struct
  type node = File of int | Directory of (string * node) list
  [@@deriving sexp]

  let root = Directory []

  let rec append_node fs (name, node) ~path =
    match (fs, path) with
    | Directory content_list, [] -> Directory ((name, node) :: content_list)
    | File _, _ -> fs
    | Directory content_list, dirname :: rpath ->
        let new_content =
          content_list
          |> List.map ~f:(fun (sname, snode) ->
                 if String.equal sname dirname then
                   (sname, append_node snode (name, node) ~path:rpath)
                 else (sname, snode))
        in
        Directory new_content

  let from_lines lines =
    lines
    |> Sequence.fold ~init:([], root) ~f:(fun (path, fs) line ->
           match (path, line) with
           | _, Line.Command (Line.Command.Cd (Custom dirname)) ->
               (dirname :: path, fs)
           | _, Line.Command (Line.Command.Cd Root) -> ([], fs)
           | _ :: npath, Line.Command (Line.Command.Cd Back) -> (npath, fs)
           | _, Line.ListItem (Line.ListItem.Directory dirname) ->
               ( path,
                 append_node fs (dirname, Directory []) ~path:(List.rev path) )
           | _, Line.ListItem (Line.ListItem.File { name; size }) ->
               (path, append_node fs (name, File size) ~path:(List.rev path))
           | _ -> (path, fs))
    |> snd

  let rec size = function
    | File size -> size
    | Directory content ->
        content
        |> List.map ~f:snd
        |> List.map ~f:size
        |> List.reduce ~f:( + )
        |> Option.value ~default:0

  let rec to_seq = function
    | File _ as node -> Sequence.return node
    | Directory content as node ->
        Sequence.append (Sequence.return node)
          (content
          |> Sequence.of_list
          |> Sequence.map ~f:snd
          |> Sequence.map ~f:to_seq
          |> Sequence.concat)

  let is_directory = function Directory _ -> true | _ -> false
end

let run f =
  Aoc2022.run @@ fun lines ->
  lines
  |> Sequence.map ~f:Line.from_string
  |> Filesystem.from_lines
  |> f
  |> Int.to_string
