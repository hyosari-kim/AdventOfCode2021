open Lib

let filename = "input/day4/sample.txt"

let lines = Loader.read_file_to_string_list filename

let f b a =
  if a = "" then [] :: b
  else
    match b with [] -> [ [] ] | [ [] ] -> [ [ a ] ] | h :: t -> (a :: h) :: t

let rid_empty l =
  List.(
    l
    |> filter (fun i -> length i <> 0)
    |> map (fun i -> i |> filter (fun j -> j <> "")))

let parse l =
  l |> List.tl
  |> List.fold_left f [ [] ]
  |> List.filter(fun l -> List.length(l) <> 0)
  |> List.(map (fun b -> b |> map (fun r -> Str.split (Str.regexp " ") r)))
  |> List.map (fun b -> rid_empty b)
  |> List.(map(fun b -> b|> map( fun r -> r |> map int_of_string)))

(** Parse : numbers, boards *)
let numbers = lines |> List.hd |> Str.split (Str.regexp ",")
let boards = parse lines

(** Functions *)
