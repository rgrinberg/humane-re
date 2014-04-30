module String = struct
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

  let string_after s n = String.sub s n (String.length s - n)

  let rec get_srch re =
    match re.srch with
    | Some r -> r
    | None   -> re.srch <- Some (Re.compile re.re);
      get_srch re

  let search_forward re s p =
    let res = Re.exec ~pos:p (get_srch re) s in
    Re.get_ofs res 0

  let split ?(num=0) t text =
    let rec split start n =
      if start > String.length text then [] else
      if n = 1 then [string_after text start] else
        try
          let (pos, match_end) = search_forward t text start in
          String.sub text start (pos-start) :: split match_end (n-1)
        with Not_found ->
          [string_after text start] in
    if text = "" then [] else split 0 num

  let split_delim ?(num=0) t text =
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
    split 0 num

  let find_all t re =
    let matches = Re.exec
    in ()

  let find_matches ({ re ; _ } as t) s =
    let res = Re.exec re s in
    ()
end
