module List = ListLabels
module String = StringLabels
module Array = ArrayLabels

module type S = (module type of S)
open S

module Str : (Re with type str := string) = struct
  type t = {
    re: Re.t;
    mutable mtch: Re.re option;
    mutable srch: Re.re option;
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
  let search_forward re s p =
    let res = Re.exec ~pos:p (get_srch re) s in
    Re.get_ofs res 0

  (* TODO not tail recursive *)
  let split ?(max=0) t text =
    let rec split start n =
      if start > String.length text then [] else
      if n = 1 then [string_after text start] else
        try
          let (pos, match_end) = search_forward t text start in
          String.sub text start (pos-start) :: split match_end (n-1)
        with Not_found ->
          [string_after text start] in
    if text = "" then [] else split 0 max

  (* TODO not tail recursive *)
  let split_delim ?(max=0) t text =
    let rec split start n =
      if start >= String.length text then [] else
      if n = 1 then [`Text (string_after text start)] else
        try
          let (pos, match_end) = search_forward t text start in
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

  module Match = struct
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
      try Some matches.(i)
      with Not_found -> None

    let group t i =
      match group_pos t i with
      | None -> None
      | Some (pos, stop) -> 
        Some (String.sub t.string ~pos ~len:(stop - pos))

    let alli t =
      let rec loop acc i =
        if i = 0 then acc
        else
          match group t i with
          | None -> loop acc (pred i)
          | Some s -> loop ((i, s)::acc) (pred i)
      in loop [] (Array.length t.matches - 1)

    let all t = t |> alli |> List.map ~f:snd

    let fold_left t ~init ~f = failwith "TODO"

    let full_match_pos { matches ; _ } = matches.(0)

    let full_match { string ; matches } =
      let (pos, stop) = matches.(0) in
      String.sub string ~pos ~len:(stop - pos)
  end

  let fold_left_matches t str ~init ~f =
    let rec loop acc pos =
      try
        let res = Re.exec ~pos (get_srch t) str in
        let (_, new_pos) = Re.get_ofs res 0 in
        let match_t = res |> Re.get_all_ofs |> Match.of_offsets str in
        loop (f acc match_t) new_pos
      with Not_found -> acc
    in loop init 0

  let find_matches t s =
    fold_left_matches t s ~init:[] ~f:(fun acc match_ ->
      match_::acc)

  let find_all t str =
    str
    |> find_matches t
    |> List.map ~f:(fun m -> m |> Match.all)
    |> List.concat

  let replace_all_group t s ~f = failwith "TODO"

  let replace_all t s ~f = failwith "TODO"

end
