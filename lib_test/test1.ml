open OUnit2

module Str = Humane_re.Str

let printer strings = 
  "[" ^ (String.concat " " strings) ^ "]"

let test_split_simple _ =
  let re = Str.regexp "_\\| " in
  let str = "test_me one foo bar" in
  let s = ["test"; "me"; "one"; "foo"; "bar"] in
  let s' = Str.split re str in
  assert_equal s s' ~printer

let test_find_matches _ =
  let re = Str.regexp "[0-9]+" in
  let str = "123 456 789 testing 000" in
  let s = ["123";"456";"789";"000"] in
  let s' = Str.find_matches re str in
  assert_equal s s' ~printer

let test_fixtures =
  "test Humane_re.Str" >:::
  [
    "test split simple" >:: test_split_simple;
    "test find matches" >:: test_find_matches;
  ]

let _ = run_test_tt_main test_fixtures
