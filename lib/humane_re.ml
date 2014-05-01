module List = ListLabels
module String = StringLabels
module Array = ArrayLabels

module type S = sig
  type t
  type str

  module Match : sig
    type t
    type index

    val get : t -> index -> string option
    val get_pos : t -> index -> (int * int) option

    val fold_left : t -> init:'a -> 
      f:('a -> pos:(int * int) -> str:str -> 'a) -> 'a

    val all_matched : t -> string list
    val all : t -> string option list
  end

  val regexp : string -> t
  val quote : str -> t
  val matches : t -> str -> bool

  val split : ?max:int -> t -> str -> str list
  val split_delim : ?max:int -> t -> str -> [`Text of str | `Delim of str] list

  val fold_left_matches : t -> str -> init:'a -> f:('a -> Match.t -> 'a) -> 'a
  val find_matches : t -> str -> Match.t list
  val find_all : t -> str -> string list

  val replace_all_group : t -> str -> 
    f:(Match.t -> [`Replace of str | `Keep ] list) -> str
  val replace_all : t -> str -> f:(str -> str) -> str

  module Infix : sig
    val (=~) : str -> t -> bool
  end
end


module Str : (S with type str := string) = struct
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
    type t = {
      string: str;
      matches: ((int * int) option) array;
    }
    type index = int
    let of_offsets string offsets =
      { string ; 
        matches=(Array.map offsets ~f:(fun (x, y) ->
          if x = -1 then None else Some (x,y))) }

    let get t i = failwith "TODO"
    let get_pos t i = failwith "TODO"
    let all_matched t = []
    let all t = []
    let fold_left t ~init ~f = failwith "TODO"
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
      acc @ (Match.all_matched match_))

  let find_all t str =
    str
    |> find_matches t
    |> List.map ~f:(fun m -> m |> Match.all_matched |> List.map ~f:snd)
    |> List.concat

  let replace_all t s ~f = failwith "TODO"
  let replace_all_group t s ~f = failwith "TODO"
end
