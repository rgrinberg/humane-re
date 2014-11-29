module List   = ListLabels
module String = StringLabels
module Array  = ArrayLabels

type t = {
  re: Re.t;
  mutable mtch: Re.re option; (* match re; we prepend '^' *)
  mutable srch: Re.re option; (* search re, says as is *)
}

type str = string

let regexp s =
  { re = Re_emacs.re ~case:true s;
    mtch = None;
    srch = None }

let quote s =
  let len = String.length s in
  let buf = String.create (2 * len) in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    match s.[i] with
      '[' | ']' | '*' | '.' | '\\' | '?' | '+' | '^' | '$' as c ->
      buf.[!pos] <- '\\'; buf.[!pos + 1] <- c; pos := !pos + 2
    | c ->
      buf.[!pos] <- c; pos := !pos + 1
  done;
  (String.sub buf 0 !pos) |> regexp
;;

let rec get_mtch re =
  match re.mtch with
  | Some r -> r
  | None   -> re.mtch <- Some (Re.compile (Re.seq [Re.start; re.re]));
    get_mtch re

let matches re s =
  try
    ignore (Re.exec ~pos:0 (get_mtch re) s);
    true
  with Not_found -> false

module Infix = struct
  let (=~) s re = matches re s
end

let string_after s n = String.sub s n (String.length s - n)

let rec get_srch re =
  match re.srch with
  | Some r -> r
  | None   -> re.srch <- Some (Re.compile re.re);
    get_srch re

(* searches forward and  *)
let search_forward_exn re s p =
  let res = Re.exec ~pos:p (get_srch re) s in
  Re.get_ofs res 0

let search_forward re s p =
  try Some (search_forward_exn re s p)
  with Not_found -> None

(* TODO not tail recursive *)
let split ?(max=0) t text =
  let rec split start n =
    if start > String.length text then [] else
    if n = 1 then [string_after text start] else
      try
        let (pos, match_end) = search_forward_exn t text start in
        String.sub text start (pos-start) :: split match_end (n-1)
      with Not_found ->
        [string_after text start] in
  if text = "" then [] else split 0 max
;;

(* Not tail recursive, since max is assumed to be small *)
let split_delim ?(max=0) t text =
  let rec split start n =
    if start >= String.length text then [] else
    if n = 1 then [`Text (string_after text start)] else
      try
        let (pos, match_end) = search_forward_exn t text start in
        let s = String.sub text pos (match_end - pos) in
        if pos > start then
          `Text (String.sub text start (pos-start)) ::
          `Delim (s) ::
          split match_end (n-1)
        else
          `Delim (s) :: split match_end (n-1)
      with Not_found ->
        [`Text (string_after text start)] in
  split 0 max
;;

let bool_of_option = function
  | None -> false
  | Some _ -> true

let fold_split re text ~init ~f =
  let find_first re position =
    try
      let token =
        match search_forward re text position with
        | None -> false
        | Some _ -> true in
      Some (token, search_forward_exn re text position)
    with Not_found -> None (* List.find fails *)
  in
  let sub start end_ =
    object
      method pos = (start, end_)
      method str = String.sub text ~pos:start ~len:(end_ - start)
    end in
  let rec split start acc =
    if start >= String.length text then acc else
      match find_first re start with
      | None ->
        let s = sub start (String.length text - 1) in
        f acc (`Delim s)
      | Some (token, (pos, match_end)) ->
        let s = sub pos match_end in
        if pos > start then
          let str = `Token (sub start pos) in
          let delim  = `Delim (s) in
          split match_end (f (f acc delim) str)
        else
          split match_end (f acc (`Delim s))
  in split 0 init
;;

module Group = struct
  type str = string
  type t = {
    string: str;
    (* first element is the coordinate of the full match. all groups
       included. This is not really the cleanest representation but we reuse
       it to avoid copying *)
    matches: (int * int) array;
  }
  type index = int

  let of_offsets string matches = { string ; matches }

  let group_pos { matches ; _ } i =
    try
      let m = matches.(i) in
      if fst m = -1
      then None
      else Some m
    with Not_found -> None

  let group t i =
    match group_pos t i with
    | None -> None
    | Some (pos, stop) ->
      Some (String.sub t.string ~pos ~len:(stop - pos))

  let group_exn t i =
    match group t i with
    | None -> raise Not_found
    | Some x -> x

  let group_substr t i =
    match group_pos t i with
    | None -> None
    | Some (pos, stop) ->
      Some (object
        method pos = (pos, stop)
        method str = String.sub t.string ~pos ~len:(stop - pos)
      end)

  let alli t =
    let rec loop acc i =
      if i = 0 then acc
      else
        match group t i with
        | None -> loop acc (pred i)
        | Some s -> loop ((i, s)::acc) (pred i)
    in loop [] (Array.length t.matches - 1)

  let all t = t |> alli |> List.map ~f:snd

  let some_exn = function
    | Some x -> x
    | _ -> invalid_arg "some_exn"

  let fold_left ({ string ; matches } as t) ~init ~f =
    let acc = ref init in
    for i = 1 to Array.length matches - 1 do
      let pos = matches.(i) in
      if fst pos <> -1 then
        acc := f !acc
                 (object
                   method pos = matches.(i)
                   method str = some_exn (group t i)
                 end)
    done;
    !acc
  ;;

  let full_match_pos { matches ; _ } = matches.(0)

  let full_match { string ; matches } =
    let (pos, stop) = matches.(0) in
    String.sub string ~pos ~len:(stop - pos)

end

let fold_left_groups t str ~init ~f =
  let rec loop acc pos =
    try
      let res = Re.exec ~pos (get_srch t) str in
      let (_, new_pos) = Re.get_ofs res 0 in
      let match_t = res |> Re.get_all_ofs |> Group.of_offsets str in
      loop (f acc match_t) new_pos
    with Not_found -> acc
  in loop init 0

let fold_left_match t str ~init ~f =
  fold_left_groups t str ~init ~f:(fun acc match_ ->
    f acc @@ object
      method pos = Group.full_match_pos match_
      method str = Group.full_match match_
    end)

let find_groups t str =
  fold_left_groups t str ~init:[] ~f:List.(fun acc x -> x::acc)
  |> List.rev

let find_matches t s =
  fold_left_match t s ~init:[]
    ~f:(fun acc match_ -> (match_ # str)::acc)
  |> List.rev

let find_concat_groups t str =
  str
  |> find_groups t
  |> List.map ~f:Group.all
  |> List.concat

let search_forward ?(start=0) re str =
  try
    let res = Re.exec ~pos:start (get_srch re) str in
    Some (object (self)
      val pos = Re.get_ofs res 0
      method pos = pos
      method str = String.sub str ~pos:(fst pos) ~len:((snd pos) - (fst pos))
    end)
  with Not_found -> None
