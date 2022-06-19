module type Board = sig
  type coord
  type board
  type t

  val make : string -> t
  val find : int -> board -> coord option
  val get_number : coord -> board -> int
  val draw : t -> int -> t
end

module Board : Board = struct
  type coord = int * int
  type board = int array array
  type t = { input : board; marked : coord list }

  let make str =
    let input =
      str
      |> Str.split (Str.regexp "\n")
      |> List.map (Str.split (Str.regexp " "))
      |> List.map (fun l -> List.map int_of_string l)
      |> List.map Array.of_list |> Array.of_list
    in
    { input; marked = [] }

  let rec find_in_list (n : int) (loc : int) = function
    | [] -> None
    | h :: t -> if h == n then Some loc else find_in_list n (loc + 1) t

  let rec find_in_list_of_list (n : int) (loc : int) = function
    | [] -> None
    | h :: t ->
        let col = find_in_list n 0 h in
        if Option.is_some col then Option.map (fun i -> (loc, i)) col
        else find_in_list_of_list n (loc + 1) t

  let find (n : int) (board : board) : coord option =
    let lboard = board |> Array.(map to_list) |> Array.to_list in
    find_in_list_of_list n 0 lboard

  let get_number (coord : coord) (b : board) =
    let y, x = coord in
    let row = Array.get b y in
    Array.get row x

  let draw (state : t) (n : int) =
    let coord = find n state.input in
    let result =
      coord
      |> Option.map (fun c ->
             { input = state.input; marked = c :: state.marked })
    in
    Option.value result ~default:state
end
