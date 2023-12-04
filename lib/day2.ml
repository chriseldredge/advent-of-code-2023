type sample = { color: string; count: int }

type game = { num: int; samples: sample list list}

(* Game 1: 5 red, 1 green; 6 red, 3 blue; 9 red; 1 blue, 1 green, 4 red; 1 green, 2 blue*)
let parse line =
  let parse_s s =
    Scanf.sscanf s "%d %s" (fun n c -> { color=c; count=n}) in
  let parse_sample s =
    s
    |> String.split_on_char ','
    |> List.map String.trim
    |> List.map parse_s in

  let parse_samples s =
    s |> String.split_on_char ';' |> List.map parse_sample
  and parts = String.split_on_char ':' line in
  match parts with
  | head :: rest :: [] -> begin
    let num = Scanf.sscanf head "Game %d" (fun n -> n) in
    { num=num; samples=parse_samples rest }
  end
  | _ -> assert false

let%test _ = parse "Game 52: 2 red, 1 blue; 7 green" =
  { num = 52; samples = [[{color="red"; count=2}; {color="blue"; count=1}]; [{color="green"; count=7}]] }

let valid most =
  let predicate game =
    let valid1 sample =
      try
        let found = List.find (fun s -> s.color = sample.color) most in
        found.count >= sample.count
      with Not_found -> begin
        print_endline ("Not found: " ^ sample.color);
        false
      end in
    List.for_all (fun sets -> List.for_all valid1 sets) game.samples in
    predicate

let part1 input =
  let x = Util.read_file input
  |> List.map parse
  |> List.filter (valid [{color="red"; count=12}; {color="green"; count=13}; {color="blue"; count=14}])
  |> List.map (fun g -> g.num)
  |> List.fold_left (+) 0 in
  x

let max_merge a b =
  let rec max_merge_rec a b lst =
    match a with
    | [] -> lst
    | x :: xs -> begin
      try
        let found = List.find (fun y -> x.color = y.color) b in
        let m = { color = x.color; count = (max x.count found.count) } in
        max_merge_rec xs b (m :: lst)
      with Not_found ->
        max_merge_rec xs b (x :: lst)
      end in
  let rec insert_distinct a lst =
    match a with
    | [] -> lst
    | x :: xs -> begin
      try
        let _ = List.find (fun y -> x.color = y.color) lst in
        insert_distinct xs lst
      with Not_found ->
        insert_distinct xs (x :: lst)
      end in
  let r = max_merge_rec a b [] in
  let f = insert_distinct b r in
  f
(*  (List.iter (fun s -> Printf.printf "%s %d\n" s.color s.count) f; f)*)

(*
let%test _ = max_merge [{color="red"; count=2}] [{color="red"; count=5}] =
  [{color="red"; count=5}]
*)

let%test _ = max_merge [{color="red"; count=2}; {color="blue"; count=1}] [{color="red"; count=5}; {color="green"; count=3}] =
  [ {color="green"; count=3}; {color="blue"; count=1}; {color="red"; count=5}]

let max_per_color_all sets =
  let a = List.fold_left (max_merge) [] sets in
  (List.iter (fun s -> Printf.printf "%s %d; " s.color s.count) a; Printf.printf "\n"; a)

let part2 input =
  let x = Util.read_file input
  |> List.map parse
  |> List.map (fun g -> max_per_color_all g.samples)
  |> List.map (fun s -> List.map (fun g -> g.count) s)
  |> List.map (fun s -> List.fold_left ( * ) 1 s)
  |> List.fold_left (+) 0 in
  x

let solve input = begin
  print_endline (string_of_int (part1 input));
  print_endline (string_of_int (part2 input))
end
