type card = { head: string; win_nums: int list; nums: int list }

let print_int_list lst =
  let meat=List.fold_left (fun a b -> a ^ (if a = "" then "" else "; ") ^ b) "" (List.map string_of_int lst) in
  "[" ^ meat ^ "]"

let print_card card =
  Printf.sprintf "%s %s | %s" card.head (print_int_list card.win_nums) (print_int_list card.nums)

let parse s =
  let parse_nums s =
    String.split_on_char ' ' s
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
    |> List.map int_of_string in
  let parse_cells cells =
    String.split_on_char '|' cells
    |> List.map parse_nums in
  match String.split_on_char ':' s with
  | [ head; cells ] -> (
    match parse_cells cells with
    | win_nums :: nums :: [] -> { head; win_nums; nums}
    | _ -> invalid_arg "parse error")
  | _ -> invalid_arg "parse error"

let rec intersect a b acc f =
  let rec contains test lst =
    match lst with e :: es -> if test e then true else contains test es | [] -> false in
  match a with
  | n :: ns ->
    let nacc=if contains (fun q -> q=n) b then (f n acc) else acc in
    intersect ns b nacc f
  | [] -> acc

let part1 input =
  let items = Util.read_file input |> List.map parse in (
    List.map (fun c -> intersect c.win_nums c.nums 0 (fun _ acc -> if acc = 0 then 1 else acc*2)) items
    |> List.fold_left (+) 0
  )

let rec count_cards (cards:card list) (copies:int list) sum =
  match cards with
  | c :: cs -> begin
    match copies with
    | cp :: cps -> begin
      let wins=intersect c.win_nums c.nums 0 (fun _ acc -> acc + 1) in
      count_cards
        cs
        (List.mapi (fun i v -> if i < wins then v+(cp+1) else v) cps)
        (sum+cp+1)
    end
    | _ -> failwith "stack empty"
  end
  | [] -> sum

let part2 input =
  let items = Util.read_file input |> List.map parse in (
    count_cards items (List.map (fun _ -> 0) items) 0
)

let solve input =
  print_endline (string_of_int (part1 input));
  print_endline (string_of_int (part2 input))
