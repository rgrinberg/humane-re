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

let test_fixtures =
  "test Humane_re.Str" >:::
  [
    "test split simple" >:: test_split_simple;
  ]

let _ = run_test_tt_main test_fixtures
