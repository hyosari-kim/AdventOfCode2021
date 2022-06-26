(* --- Day 2: Dive! --- *)
open Lib

let filename = "input/day2/puzzle.txt"
let lines = Loader.read_file_to_string_list filename

type cmd = Forward of int | Down of int | Up of int

let parse str = 
  match str with
    | ["forward"; i] -> Forward (int_of_string i)
    | ["down"; i] -> Down (int_of_string i)
    | ["up"; i] -> Up (int_of_string i)
    | _ -> raise (Failure "parse error")

let commands = lines |> List.map(String.split_on_char ' ') |> List.map(parse)

(* part 1. 
   Get final hoizontal and depth position and then multiply them. *)

(* workflow
  string list -> (command, position) list -> (horiz_position, depth) -> result *)

let compute pos cmd =
  let (fow, depth ) = pos in
    match cmd with
    | Forward i -> (fow + i, depth)
    | Down i -> (fow, depth + i)
    | Up i -> (fow, depth - i)

let position = commands |> List.fold_left compute (0, 0)

let result = let (pos, depth) = position in pos * depth

let _ = print_string ("part1: " ^ string_of_int result)

(* part 2. 
   Ops!. We need to track aim. It is affected by "up/down" and need to be considered in "forward". 
   Get final hoizontal and depth position and then multiply them. *)

(* workflow
  It just needs to change the compute function that returns horizontal and depth *)

let compute2 pos cmd =
  let (fow, depth, aim) = pos in
    match cmd with
      | Forward i -> (fow + i, depth + (i * aim), aim)
      | Down i -> (fow, depth, aim + i)
      | Up i -> (fow, depth, aim - i)

let position2 = commands |> List.fold_left compute2 (0, 0, 0)

let result2 = let (pos, depth, _) = position2 in pos * depth

let _ = print_string ("part2: " ^ string_of_int result2)
