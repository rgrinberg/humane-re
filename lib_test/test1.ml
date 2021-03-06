open OUnit2

module Str = Humane_re.Str

let quote s = "\"" ^ s ^ "\""
let printer strings = "[" ^ (String.concat " " (List.map quote strings)) ^ "]"

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

let test_find_concat_groups _ =
  let re = Str.regexp "\\([0-9]+\\)_\\([0-9]+\\)" in
  let str = "123_789 testin one two 000_111 foobar" in
  let s = ["123"; "789"; "000"; "111"] in
  let s' = Str.find_concat_groups re str in
  assert_equal s s' ~printer

let test_find_groups _ =
  let re = Str.regexp "\\([0-9][0-9]\\) \\([a-z]+\\)" in
  let str = "12 fruit 15 apples XXX YYY 19 things" in
  let groups = Str.find_groups re str in
  assert_equal (List.length groups) 3 ~printer:string_of_int;
  let s = [("12 fruit", "12", "fruit");
           ("15 apples", "15", "apples");
           ("19 things", "19", "things")] in
  let s' = groups |> List.map (fun g ->
    let fm = Str.Group.full_match g in
    match Str.Group.all g with
    | [x ; y] -> (fm, x, y)
    | _ -> assert_failure "did not match 2 elems"
  ) in
  assert_equal s s'

let test_fold_split1 _ =
  let test_string = "test:123456 one:456 four:xxx" in
  let re = Str.regexp "[: ]" in
  let all_tokens = ["test"; "123456"; "one"; "456"; "four"; "xxx"] in
  let fs = Str.fold_split re test_string ~init:[]
             ~f:(fun acc t ->
               match t with
               | `Delim _ -> acc
               | `Token sub -> (sub # str)::acc) |> List.rev in
  assert_equal all_tokens fs ~printer

let test_bug_1 _ =
  let re = Humane_re.Str.regexp "x" in
  let res =
    Humane_re.Str.fold_split re "axbxc" ~init:[] ~f:(fun acc ->
      function
      | `Delim s
      | `Token s -> s#str :: acc) in
  assert_equal ["c"; "x"; "b"; "x"; "a"] res ~printer

let test_fixtures =
  "test Humane_re.Str" >:::
  [
    "test split simple" >:: test_split_simple;
    "test find matches" >:: test_find_matches;
    "test find concat groups" >:: test_find_concat_groups;
    "test find groups" >:: test_find_groups;
    "test fold split1" >:: test_fold_split1;
    "test bug 1 - fold split" >:: test_bug_1
  ]

let _ = run_test_tt_main test_fixtures
