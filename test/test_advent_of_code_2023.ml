open Advent_of_code_2023.Day2
open OUnit2

let test_max_merge _ =
  let actual = Advent_of_code_2023.Day2.max_merge [{color="red"; count=2}; {color="blue"; count=1}] [{color="red"; count=5}; {color="green"; count=3}]
  and expected = [ {color="green"; count=3}; {color="blue"; count=1}; {color="red"; count=5} ] in
  assert_equal actual expected


let () =
  run_test_tt_main
    ("factorial tests" >:::
       [
         "factorial of zero" >:: test_max_merge;
       ])

