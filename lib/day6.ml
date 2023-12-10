let print_int_list lst =
  let meat=List.fold_left (fun a b -> a ^ (if a = "" then "" else "; ") ^ b) "" (List.map string_of_int lst) in
  "[" ^ meat ^ "]"

let parse_nums s =
  String.split_on_char ' ' s
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string

  let parse_num_with_spaces s =
  String.split_on_char ' ' s
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.fold_left (^) ""
  |> int_of_string

let parse_after_label f s =
  match String.split_on_char ':' s with
  | _ :: nums :: [] -> f nums
  | _ -> failwith "parse error"

let parse1 lines =
  List.map (parse_after_label parse_nums) lines

let rec count_wins i n d acc =
  if i<n then
    let w = if i*(n-i) > d then 1 else 0 in
    count_wins (i+1) n d (acc + w)
  else
    acc

let count_wins_product data =
  match data with
  | times :: distances :: [] ->
    List.map2 (count_wins 0) times distances
    |> List.map (fun f -> f 0)
    |> List.fold_left ( * ) 1
  | _ -> failwith "parse error"

let part1 input =
  input
  |> List.map (parse_after_label parse_nums)
  |> count_wins_product

let part2 input =
  let vals = input
    |> List.map (parse_after_label parse_num_with_spaces) in
    match vals with
    | t :: d :: [] -> count_wins 0 t d 0
    | _ -> invalid_arg "input must be two numbers"

let solve input = (
  let data = Util.read_file input in (
    print_endline (string_of_int (part1 data));
    print_endline (string_of_int (part2 data));
  )
)