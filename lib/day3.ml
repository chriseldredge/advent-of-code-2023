type point = {x: int; y: int}

type item =
  | Number of {value: int; loc: point; len: int}
  | Symbol of {sym: char; loc: point}

let get_sym chars loc =
  let digit_to_int c = (int_of_char c) - (int_of_char '0') in
  let rec get_num lst base acc len = match lst with
    | c :: cs -> get_num cs (base * 10) (((digit_to_int c) * base) + acc) (len + 1)
    | [] -> Number {value=acc; loc; len} in
  let x:item = match chars with
  | c :: _ when c >= '0' && c <= '9' -> get_num chars 1 0 0
  | c :: _ -> Symbol {sym=c; loc}
  | _ -> assert false in
  x

let push_sym sym loc lst = match sym with
  | [] -> lst
  | _ -> (get_sym sym loc) :: lst

let parse_row (row:string) y =
  let rec parse_row_rec lst x y sym sym_x acc = match lst with
    | c :: cs when c >= '0' && c <= '9' -> parse_row_rec cs (x + 1) y (c :: sym) sym_x acc
    | '.' :: cs -> parse_row_rec cs (x+1) y [] (x+1) (push_sym sym {x=sym_x; y=y} acc)
    | c :: cs -> parse_row_rec cs (x+1) y [] (x+1) (push_sym [c] {x=x; y=y} (push_sym sym {x=sym_x; y=y} acc))
    | [] -> (push_sym sym {x=sym_x; y=y} acc)
  and explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [] in
  List.rev (parse_row_rec (explode row) 0 y [] 0 [])

let parse_rows (rows: string list) =
  let rec parse_rows_rec (lst:string list) y acc = match lst with
  | r :: rs -> parse_rows_rec rs (y+1) ((parse_row r y) :: acc)
  | [] -> acc in
  parse_rows_rec rows 0 []

let print_sym_list (lst:item list) =
  let print_item i = match i with
  | Number s -> Printf.sprintf "{num=%d loc={%d %d} len=%d}" s.value s.loc.x s.loc.y s.len
  | Symbol s -> Printf.sprintf "{sym=%c loc={%d %d}}" s.sym s.loc.x s.loc.y in
  List.map print_item lst
  |> List.fold_left (fun a b -> a ^ (if a = "" then "" else "; ") ^ b) ""

let adj num sym = match num with
| Number n -> (match sym with
| Symbol s -> abs (n.loc.y - s.loc.y) < 2 && s.loc.x >= (n.loc.x-1) && s.loc.x <= (n.loc.x+n.len)
| _ -> false)
| _ -> false

let rec any_adj num syms = match syms with
| s :: ss -> if adj num s then true else any_adj num ss
| [] -> false

let find_part_numbers nums syms =
  let rec find_r nums syms acc = match nums with
  | n :: ns -> if any_adj n syms then find_r ns syms (n :: acc) else find_r ns syms acc
  | [] -> acc in
  find_r nums syms []

let part1 input =
  let items = Util.read_file input
  |> parse_rows
  |> List.rev
  |> List.flatten in
  let syms = List.filter (fun i -> match i with Symbol _ -> true | _ -> false) items
  and nums = List.filter (fun i -> match i with Number _ -> true | _ -> false) items in
  let part_nums = begin
    (*
    print_endline (print_sym_list nums);
    print_endline (print_sym_list syms);
    *)
    find_part_numbers nums syms;
  end in
  let vals = List.map (fun pn -> match pn with Number n -> n.value | _ -> 0) part_nums in
  List.fold_left (+) 0 vals

  let find_adj_nums nums s =
    List.filter (fun n -> adj n s) nums

  let part2 input =
    let items = Util.read_file input
    |> parse_rows
    |> List.rev
    |> List.flatten in
    let stars = List.filter (fun i -> match i with Symbol s -> s.sym = '*' | _ -> false) items
    and nums = List.filter (fun i -> match i with Number _ -> true | _ -> false) items in
    let rec adj_to_syms syms nums acc = match syms with
    | s :: ss -> adj_to_syms ss nums ((find_adj_nums nums s) :: acc)
    | _ -> acc in
    let nums_adj_to_syms = adj_to_syms stars nums [] in
    let rec gear_powers nums_list acc = match nums_list with
    | nl :: nls -> (match nl with
      |Number a :: Number b :: [] -> gear_powers nls (a.value*b.value :: acc)
      | _ -> gear_powers nls acc)
    | _ -> acc in
    begin
      (*List.iter (fun x -> print_endline (print_sym_list x)) (gear_powers nums_adj_to_syms);*)
      List.fold_left (+) 0 (gear_powers nums_adj_to_syms [])
    end

  let solve input = begin
    print_endline (string_of_int (part1 input));
    print_endline (string_of_int (part2 input))
  end
