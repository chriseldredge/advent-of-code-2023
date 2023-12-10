let parse_nums s =
  String.split_on_char ' ' s
  |> List.map String.trim
  |> List.filter ((<>) "")
  |> List.map int_of_string

let rec parse_lines lines acc =
  match lines with l :: ls -> parse_lines ls (parse_nums l :: acc) | [] -> acc

let rec diff_nums nums =
  match nums with
  | n1 :: n2 :: ns -> (n2 - n1) :: diff_nums (n2 :: ns)
  | _ :: _ -> []
  | _ -> failwith "illegal state"

let rec diff_until_zero nums acc =
  if List.for_all (( = ) 0) nums then acc
  else diff_until_zero (diff_nums nums) (nums :: acc)

let next_in_seq nums =
  diff_until_zero nums []
  |> List.map (fun n -> List.hd (List.rev n))
  |> List.fold_left ( + ) 0

let prev_in_seq nums =
  let rec prev_in_seq_rec diffs prev =
    match diffs with
    | n :: ns -> prev_in_seq_rec ns (List.hd n - prev)
    | [] -> prev
  in
  prev_in_seq_rec (diff_until_zero nums []) 0

let part1 input =
  parse_lines input [] |> List.map next_in_seq |> List.fold_left ( + ) 0

let part2 input =
  parse_lines input [] |> List.map prev_in_seq |> List.fold_left ( + ) 0

let solve input =
  let data = Advent_of_code_2023.Util.read_file input in
  print_endline (string_of_int (part1 data));
  print_endline (string_of_int (part2 data))

let () = solve Sys.argv.(1)
