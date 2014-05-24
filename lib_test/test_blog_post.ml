open OUnit2
module Str = Humane_re.Str

let is_valid_email =
  let email_re = Str.regexp ".+@.+" in
  let open Str.Infix in fun email ->
    email =~ email_re

let extract_words = Str.(find_matches (regexp "\\b\\([A-Za-z]+\\)\\b"))

let parse_header =
  let open Str in
  let re = regexp ":[ \t]*" in
  fun header ->
    match split ~max:2 re header with
    | [name; value] -> Some (name, value)
    | _ -> None

let test_emails _ =
  let test_cases = [
    ("", false);
    ("dont@spam.me", true);
    ("bill@gates.com", true);
    ("xxx.yyy@zzz.qqq.com", true);
    ("@xxx.com", false);
    ("yyy@", false);
  ] in
  test_cases |> List.iter (fun (email, result) ->
    assert_equal (is_valid_email email) result)

let extract_imgur_links page =
  let is_imgur s =
    let open Str.Infix in
    s =~ (Str.regexp ".+\\bimgur\\.com.+")
  in
  let re = Str.regexp "<a href=\"\\([^\"]+\\)\">\\([^<>]+\\)</a>" in
  page
  |> Str.fold_left_groups re ~init:[]
       ~f:(fun acc g ->
         match Str.Group.all g with
         | [href; text] when is_imgur href -> (href, text)::acc
         | _ -> acc)
  |> List.rev

let list_strings_tuples l =
  l
  |> List.map (fun (x, y) -> Printf.sprintf "(%s, %s)" x y)
  |> String.concat "\n"

let test_extract _ =
  let page = "
    <a href=\"http://imgur.com/xxx.jpg\">text 1</a> <a href=\"http://imgur.com/gif.jpg\">text 2</a>
    <a href=\"http://facebook.com/xxx.gif\">text 3</a>
    <b>random tag</b>
    <a href>invalid link</a>
    <a href=\"http://imgur.com/png.jpg\">imgur</a>" in
  let links = extract_imgur_links page in
  assert_equal links ~printer:list_strings_tuples
    [
      ("http://imgur.com/xxx.jpg", "text 1");
      ("http://imgur.com/gif.jpg", "text 2");
      ("http://imgur.com/png.jpg", "imgur");
    ]

let test_words _ =
  let words = "Lorem ipsum dolor sit amet, consectetur adipisicing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua." in
  let words_num = List.length (extract_words words) in
  assert_equal words_num 19

let test_headers _ =
  let headers = [
    ("User-Agent: firefox", "User-Agent", "firefox");
    ("XXX:", "XXX", "");
    ("Some-Header: one:two", "Some-Header", "one:two");
  ] in
  headers |> List.iter (fun (header, k, v) ->
    assert_equal (parse_header header) (Some (k, v)));
  let none_headers = [
    "User-Agent firefox";
    "User-Agent";
    "";
  ] in
  none_headers |> List.iter (fun x -> assert_equal (parse_header x) None)

let test_fixtures =
  "test Humane_re.Str blog examplesk" >:::
  [
    "test validate emails" >:: test_emails;
    "test extract words" >:: test_words;
    "test parse headers" >:: test_headers;
    "test extract links" >:: test_extract;
  ]

let _ = run_test_tt_main test_fixtures
