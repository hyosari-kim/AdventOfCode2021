open Stdlib 

let rec read_line_rev ic lines =
  let line = In_channel.input_line ic in
  match line with
  | None -> lines 
  | Some str -> read_line_rev ic (str :: lines)

let read_file filename =
  let in_ch = In_channel.open_text filename in
  read_line_rev in_ch []

let read_file_to_string_list filename = read_file filename |> List.rev
