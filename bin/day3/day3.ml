open Lib

let filename = "input/day3/puzzle.txt"

let lines = Loader.read_file_to_string_list filename

let split lst =
  lst |> List.map (fun line -> Str.split (Str.regexp "") line)

  let tl_opt = function
  | [] -> None
  | _ :: t -> Some t 

let hd_opt lst = List.nth_opt lst 1

let map2 a b f = Option.bind a (fun a' -> Option.map (fun b' -> f a' b') b)

let get_nth_col (lst: int list list) n  = List.(fold_right (fun (l:int list) acc -> (nth_opt l n) :: acc) lst [] |> filter_map(Fun.id))

let remove_first_col (lst:int list list) = lst |> List.map (tl_opt)  |> List.filter_map(Fun.id)

let rec adjust_shape lst =
  match lst with
    | [] -> []
    | _ :: _ -> get_nth_col lst 0 :: adjust_shape (remove_first_col lst)

let change_shape lst = lst |> adjust_shape |> List.(filter(fun l -> length l > 1))  

(*  Parse :string list -> int list , [x,y] -> [y,x] *)
let data = lines |> split |> List.( map(fun l -> l |> map int_of_string)) |> change_shape

(*  Functions *)
let frequent_bit cmp lst = 
  lst 
    |> List.fold_left (fun acc i -> if i = 0 then (fst acc +1, snd acc) else (fst acc, snd acc +1)) (0,0) 
    |> cmp

let gamma lst = lst |> List.map(fun l -> (frequent_bit (fun (z,o) -> if z > o then 0 else 1) l) mod 2)
let epsilon lst = gamma lst |> List.map (fun i -> i lxor 1)

let decimal_of_bit (lst: int list) = 
  lst 
  |> List.map ((land) 1) 
  |> List.map float_of_int 
  |> (fun l -> List.fold_right (fun i acc -> (fst acc +. 1., snd acc +. (i *. (2. ** fst acc)))) l (0., 0.))
  |> snd

(* Part1  multiply gamma rate that is determined most common bit in the corresponing position and the epsilon rate that is determined opposite rule of gamma rate *)
let part1 = [gamma data; epsilon data] |> List.map decimal_of_bit |> List.fold_left ( *. ) 1.

(* Functions *)
let origin = lines |> split |> List.( map(fun l -> l |> map int_of_string))

type state = (int * int list list)

let step f (state:state) : state =
  let report = change_shape (snd state) in
    let cb = List.nth report (fst state) |> frequent_bit f  in
      let filtered_l = snd state 
        |> List.(filter_map (fun (l: int list) -> let bit = nth_opt l (fst state) in Option.bind bit (fun b -> if b = cb then Some(l) else None))) in
      (fst state +1 , filtered_l)

type mode = Finish of state | Continue of state
let mode_of_state (state:state) = let length = List.length(snd state) in if (length < 2) || ((fst state) > List.length(List.hd (snd state))) then Finish state else Continue state

let rec oxygen_process = function
  | Continue state -> oxygen_process (step (fun (z,o) -> if z > o then 0 else 1) state |> mode_of_state)
  | Finish state -> state

let rec co2_process = function
  | Continue state -> co2_process (step (fun (z,o) -> if z <= o then 0 else 1) state |> mode_of_state)
  | Finish state -> state
let initState = Continue (0, origin)

let oxygen = let state = oxygen_process initState in snd state  |> List.hd
let co2 = let state = co2_process initState in snd state |> List.hd

(* Part2 : multiply th oxygen generator rating and the CO2 scrubber rating *)
let part2 = [oxygen; co2] |> List.map decimal_of_bit |> List.fold_left ( *. ) 1.