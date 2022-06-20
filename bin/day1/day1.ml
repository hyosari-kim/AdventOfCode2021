open Lib

let filename = "input/day1/puzzle.txt"

let lines = Loader.read_file_to_string_list filename

let data = lines |> List.map int_of_string

let rec slidewindow (n:int) (data: int list) (acc: int list) =
  match data with
    | hd :: tl -> if n > 0 then (slidewindow (n-1) tl [hd]) |> List.append acc else acc
    | _ -> acc

let rec group n data = 
  let groups = match data with
    | _ :: tl -> group n tl @ [slidewindow n data []]
    | _ -> []
  in groups |> List.filter(fun l -> l |> List.length == n) |> List.rev


let pair (data: int list list) =
  data |> List.map(fun h -> ( h|>List.hd, List.nth h 1)) 

(*
  Part 1: count the number of times a depth measurement increases
*)

let result = data 
    |> group 2
    |> pair 
    |> List.filter(fun pair -> (snd pair - fst pair) > 0)
    |> List.length

let _ = print_string ("part1:" ^ string_of_int result)

(*
  Part 2: consider sums of a three-measurement sliding window.
*)

let sum data = data |> List.fold_left(fun acc d -> acc + d) 0

let result = data
  |> group 3
  |> List.map(fun h -> h |> sum)
  |> group 2
  |> pair
  |> List.filter(fun pair -> (snd pair - fst pair) > 0 )
  |> List.length

  let _ = print_string ("part2:" ^ string_of_int result) 
