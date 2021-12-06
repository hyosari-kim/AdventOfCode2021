open Lib

let filename = "input/day2/puzzle.txt"

let lines = Loader.read_file_to_string_list filename

(*
  Parsing:
    List<string> -> List<command>
*)

type command = Forward of int | Up of int | Down of int

let forwardExp = Str.regexp "^forward \\([0-9]+\\)$"
let upExp = Str.regexp "^up \\([0-9]+\\)$"
let downExp = Str.regexp "^down \\([0-9]+\\)$"

let get_matched_of_first rg str = Str.replace_first rg "\\1" str

let parse str = 
  if Str.string_match forwardExp str 0 then Some (Forward (int_of_string (get_matched_of_first forwardExp str)))
  else if Str.string_match upExp str 0 then Some (Up (int_of_string (get_matched_of_first upExp str)))
  else if Str.string_match downExp str 0 then Some (Down (int_of_string (get_matched_of_first downExp str)))
  else None

let map2 a b f = Option.bind a (fun a' -> Option.map (fun b' -> f a' b') b)
let sequence lst = List.fold_right (fun a acc -> map2 a acc (fun a' acc' -> a' :: acc')) lst (Some [])

let data = lines |> List.map parse |> sequence

(*
  Part 1: Calculate the horizontal position and depth you would have after following the planned course.
*)

(* state = (horizon * depth) *)
type state = int * int

let process (state:state)  = function
  | Forward i -> ((fst state)+ i , snd state)
  | Up i -> (fst state, (snd state) - i)
  | Down i -> (fst state, (snd state) +i)

let initial_state = (0,0)

let get_ifelse_init init = function
  | Some(state) -> state
  | None -> init
let final_state = data |> Option.map (List.fold_left (fun acc a -> process acc a) initial_state) |> get_ifelse_init initial_state

let _ =
  let result = string_of_int (fst final_state * snd final_state) in
  print_string ("part1:" ^ result)

(*
  Part 2: Consider aim. 
*)

(* state = horizon * depth * aim *)
type state_2 = int * int * int

let horizon ((h, _, _): state_2) = h
let depth ((_,d,_): state_2) = d
let aim ((_,_,a):state_2) = a

let process_2 (state:state_2) = function
  | Forward i -> (horizon state +i, (depth state) + ((aim state) * i), aim state)
  | Up i -> (horizon state, depth state, aim state - i)
  | Down i -> (horizon state, depth state, aim state +i)

let initial_state_2 = (0, 0, 0)
let final_state_2 = data |> Option.map(List.fold_left(fun acc a -> process_2 acc a) initial_state_2) |> get_ifelse_init initial_state_2

let _ =
  let result = string_of_int (horizon final_state_2 * depth final_state_2) in
  print_string ("part2:" ^ result)