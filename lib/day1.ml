let digit_to_int c = (int_of_char c) - (int_of_char '0');;

let rec find_digit s i d =
  if s.[i] >= '0' && s.[i] <= '9' then s.[i] else find_digit s (i+d) d;;

let find_first_number s =
  let rec help chars =
    match chars with
    | [] -> assert false
    | 'z' :: 'e' :: 'r' :: 'o' :: _ -> 0
    | 'o' :: 'n' :: 'e' :: _ -> 1
    | 't' :: 'w' :: 'o' :: _ -> 2
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> 3
    | 'f' :: 'o' :: 'u' :: 'r' :: _ -> 4
    | 'f' :: 'i' :: 'v' :: 'e' :: _ -> 5
    | 's' :: 'i' :: 'x' :: _ -> 6
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> 7
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> 8
    | 'n' :: 'i' :: 'n' :: 'e' :: _ -> 9
    | 't' :: 'e' :: 'n' :: _ -> 10
    | c :: xs -> if c >= '0' && c <= '9' then (digit_to_int c) else (help xs) in
  help (List.init (String.length s) (String.get s));;

let find_last_number s =
    let rec help chars =
      match chars with
      | [] -> assert false
      | 'o' :: 'r' :: 'e' :: 'z' :: _ -> 0
      | 'e' :: 'n' :: 'o' :: _ -> 1
      | 'o' :: 'w' :: 't' :: _ -> 2
      | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ -> 3
      | 'r' :: 'u' :: 'o' :: 'f' :: _ -> 4
      | 'e' :: 'v' :: 'i' :: 'f' :: _ -> 5
      | 'x' :: 'i' :: 's' :: _ -> 6
      | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ -> 7
      | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ -> 8
      | 'e' :: 'n' :: 'i' :: 'n' :: _ -> 9
      | 'n' :: 'e' :: 't' :: _ -> 10
      | c :: xs -> if c >= '0' && c <= '9' then (digit_to_int c) else (help xs) in
      help (List.rev (List.init (String.length s) (String.get s)));;

let first_digit s =
  find_digit s 0 1;;

let last_digit s =
    find_digit s (String.length s - 1) (-1);;

let decode line =
  (digit_to_int (first_digit line)) * 10 + (digit_to_int (last_digit line));;

let stupid_decode line =
  (find_first_number line) * 10 + (find_last_number line)

let part1 () =
  let lines = Util.read_file "/Users/chris/codes/advent-of-code-2023/input/day1.txt" in
  let decode = List.map decode lines in
  List.fold_left (+) 0 decode

let part2 () =
  let lines = Util.read_file "/Users/chris/codes/advent-of-code-2023/input/day1.txt" in
  let decode = List.map stupid_decode lines in
  List.fold_left (+) 0 decode

let solve () = begin
  print_endline (string_of_int (part1 ()));
  print_endline (string_of_int (part2 ()))
end

let%test _ = first_digit "5" = '5'
let%test _ = first_digit "a5" = '5'
let%test _ = first_digit "a5b7q9e" = '5'
let%test _ = last_digit "5" = '5'
let%test _ = last_digit "a5e7q" = '7'
let%test _ = last_digit "a5b" = '5'
let%test _ = decode "a5b7" = 57
let%test _ = find_first_number "aqone5b7" = 1
let%test _ = find_first_number "aqtwo5b7" = 2
let%test _ = find_last_number "aqtwo5b7" = 7
let%test _ = find_last_number "aqtwo5b7tenq" = 10
