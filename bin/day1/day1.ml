open Lib

let filename = "input/day1/puzzle.txt"

let lines = Loader.read_file_to_string_list filename

let data = lines |> List.map int_of_string

(*
  Part 1: count the number of times a depth measurement increases
*)

let finalState =
  data
  |> List.fold_left
       (fun acc d ->
         match acc with
         | 0, 0 -> (d, 0)
         | bef, cnt -> (d, if bef < d then cnt + 1 else cnt))
       (0, 0)

let _ =
  let result = string_of_int (snd finalState) in
  print_string ("part1:" ^ result)

(*
  Part 2: consider sums of a three-measurement sliding window.
*)

let rec sum_of_til_nth n = function
  | [] -> 0
  | h :: t -> if n > 0 then h + sum_of_til_nth (n - 1) t else 0

let rec find_increses = function
  | [] -> 0
  | h :: t ->
      if h + sum_of_til_nth 2 t < sum_of_til_nth 3 t then 1 + find_increses t
      else find_increses t

let _ =
  let result = string_of_int (find_increses data) in
  print_string ("part2:" ^ result)
