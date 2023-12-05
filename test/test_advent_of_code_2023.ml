open Advent_of_code_2023.Day2
open Advent_of_code_2023.Day3
open OUnit2

let test_max_merge _ =
  let actual = Advent_of_code_2023.Day2.max_merge [{color="red"; count=2}; {color="blue"; count=1}] [{color="red"; count=5}; {color="green"; count=3}]
  and expected = [ {color="green"; count=3}; {color="blue"; count=1}; {color="red"; count=5} ] in
  assert_equal actual expected

let test_get_sym _ =
  match (get_sym ['6'; '1'; '9'] {x=0; y=5}) with
  | Symbol _ -> assert false
  | Number n -> assert_equal n.value 916

let test_parse_row _ =
  let vals = (parse_row "...1324..$.52319" 99) in (
    assert_equal [Number {value=1324; loc={x=3;y=99}; len=4}; Symbol { sym='$'; loc={x=9; y=99 }}; Number {value=52319; loc={x=11;y=99}; len=5}] vals ~printer:print_sym_list;
  )

let () =
  run_test_tt_main
    ("day2 tests" >:::
       [
         "factorial of zero" >:: test_max_merge;
         "test_get_sym" >:: test_get_sym;
         "test_parse_row" >:: test_parse_row;
       ])

