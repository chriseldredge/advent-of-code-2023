let repeat lst =
  let rec repeat_h cur = match cur with
  | e :: es -> Seq.Cons (e, fun() -> repeat_h es)
  | [] -> repeat_h lst in
  repeat_h []

let hd = function
| Seq.Cons (h,_) -> h
| Nil -> invalid_arg "empty sequence"

let tl = function
| Seq.Cons (_, t) -> t ()
| Nil -> invalid_arg "empty sequence"

let rec take n s =
  if n = 0 then [] else hd s :: take (n - 1) (tl s)

let  explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let parse input =
  let parse_node line =
    (String.sub line 0 3, String.sub line 7 3, String.sub line 12 3) in
  let turns = (explode (List.hd input))
  and nodes = List.map parse_node (List.tl (List.tl input)) in
  let node_tbl = Hashtbl.create (List.length nodes) in
  (
    List.iter (fun i -> match i with (k, l, r) -> Hashtbl.add node_tbl k (l, r)) nodes;
    (turns, node_tbl)
  )

let rec traverse tbl stop_f turns k step =
  let v = Hashtbl.find tbl k in
  if (stop_f k) then step else
  let turn=hd turns
  and next_turns=tl turns in
  match v with
  | (l, r) -> (
      let next_k=if turn='L' then l else r in
      traverse tbl stop_f next_turns next_k (1+step))

let part1 input =
  let data = parse input in
  match data with
  | (turns, node_tbl) ->
    traverse node_tbl ((=) "ZZZ") (repeat turns) "AAA" 0

let part2 input =
  let data = parse input in
  match data with
  | (turns, node_tbl) ->
    let keys = Hashtbl.fold (fun k _ acc -> k :: acc) node_tbl [] in
    let start_ks = List.filter (String.ends_with ~suffix:"A") keys in
    List.map (fun k ->
      traverse node_tbl (String.ends_with ~suffix:"Z") (repeat turns) k 0) start_ks
    |> List.map Z.of_int
    |> List.fold_left Z.lcm (Z.of_int 1)
    |> Z.to_int

let solve input = (
  let data = Advent_of_code_2023.Util.read_file input in (
    print_endline (string_of_int (part1 data));
    print_endline (string_of_int (part2 data));
  )
)

let () = (
  solve Sys.argv.(1)
)
