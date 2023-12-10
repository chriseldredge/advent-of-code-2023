type hand = char list
type entry = { hand: hand; bid: int; hand_str: string }

let print_int_list lst =
  let meat=List.fold_left (fun a b -> a ^ (if a = "" then "" else "; ") ^ b) "" (List.map string_of_int lst) in
  "[" ^ meat ^ "]"

let  explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let parse_line s =
  match String.split_on_char ' ' s with
  | hand_str :: bid_str :: [] -> { hand=(explode hand_str); bid=(int_of_string bid_str); hand_str}
  | _ -> failwith "parse error"

let count_distinct (hand:hand) =
  let rec count_distinct_r (lst:hand) (hash:(char, int) Hashtbl.t) =
    let incr (hash:(char, int) Hashtbl.t) (k:char): unit =
      match Hashtbl.find_opt hash k with
      | Some count -> Hashtbl.replace hash k (count + 1)
      | None -> Hashtbl.add hash k 1 in
    match lst with
    | c :: cs -> (incr hash c; count_distinct_r cs hash)
    | _ -> hash
  and hash = Hashtbl.create 5 in
  count_distinct_r hand hash

let hand_strength (special:char) (h:hand) :int =
  let hsh = (count_distinct h) in
  let special_count = match Hashtbl.find_opt hsh special with
  | Some c -> c
  | None -> 0 in
  let lst = Hashtbl.fold (fun ch c acc -> if ch = special then acc else c :: acc) (count_distinct h) [] in
  match List.sort (Fun.flip compare) lst with
  | 5 :: _ -> 0
  | 4 :: _ -> 1 - special_count
  | 3 :: 2 :: _ -> 2
  | 3 :: _ -> if special_count <> 0 then 2 - special_count else 3
  | 2 :: 2 :: _ -> if special_count = 1 then 2 else 4
  | 2 :: _ -> if special_count = 3 then 0 else if special_count = 2 then 1 else if special_count = 1 then 3 else 5
  | 1 :: _ -> if special_count = 4 then 0 else if special_count = 3 then 1 else if special_count = 2 then 3 else if special_count = 1 then 5 else 6
  | _ -> 0

let default_hand_strength = hand_strength '.'
let joker_hand_strength = hand_strength 'J'

let high_cards = ['A'; 'K'; 'Q'; 'J'; 'T']
let joker_high_cards = ['A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; '1'; 'J']

let cmp_1st_card (card_vals:char list) (a:char) (b:char) :int =
  let card_val ch = match List.find_index (fun c -> c=ch) card_vals with
  | Some i -> i
  | None -> (14 - ((int_of_char ch) - (int_of_char '0'))) in
  compare (card_val a) (card_val b)

let default_cmp_1st_card = cmp_1st_card high_cards
let joker_cmp_1st_card = cmp_1st_card joker_high_cards

let compare_entries hnd_strength cmp_1st (a:entry) (b:entry): int =
  let rec compare_hands (a:hand) (b:hand): int =
    let cmp_chr = (cmp_1st (List.hd a) (List.hd b)) in
    if cmp_chr <> 0 then cmp_chr else compare_hands (List.tl a) (List.tl b)
  and cmp_strength = (compare (hnd_strength a.hand) (hnd_strength b.hand)) in
  if cmp_strength = 0 then (compare_hands a.hand b.hand) else cmp_strength

let default_compare_entries = compare_entries default_hand_strength default_cmp_1st_card
let joker_compare_entries = compare_entries joker_hand_strength joker_cmp_1st_card

let part1 input =
  input
  |> List.map parse_line
  |> List.sort (Fun.flip default_compare_entries)
  |> List.mapi (fun i s -> s.bid*(i+1))
  |> List.fold_left (+) 0

let part2 input =
  input
  |> List.map parse_line
  |> List.sort (Fun.flip joker_compare_entries)
  |> List.mapi (fun i s -> s.bid*(i+1))
  |> List.fold_left (+) 0

let solve input = (
  let data = Advent_of_code_2023.Util.read_file input in (
    print_endline (string_of_int (part1 data));
    print_endline (string_of_int (part2 data));
  )
)

let () = (
  solve Sys.argv.(1)
)
