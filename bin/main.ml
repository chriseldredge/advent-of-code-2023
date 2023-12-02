let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let lines = read_file "input/day1.txt";;

let decode = List.map Advent_of_code_2023.Day1.decode lines;;
let decode_sum = List.fold_left (+) 0 decode;;
let stupid_decode = List.map Advent_of_code_2023.Day1.stupid_decode lines;;
let stupid_decode_sum = List.fold_left (+) 0 stupid_decode;;

let () = print_endline (string_of_int decode_sum);;
let () = print_endline (string_of_int stupid_decode_sum);;
