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
let part1 =
  let x = Util.read_file "input/day2.txt"
  |> List.map parse
  |> List.filter (valid [{color="red"; count=12}; {color="green"; count=13}; {color="blue"; count=14}])
  |> List.map (fun g -> g.num)
  |> List.fold_left (+) 0 in
  x

let solve = begin
  print_endline (string_of_int part1);

end

let%test _ = parse "Game 52: 2 red, 1 blue; 7 green" =
  { num = 52; samples = [[{color="red"; count=2}; {color="blue"; count=1}]; [{color="green"; count=7}]] }
