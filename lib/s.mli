type 'a substr = < pos : int * int ; str : 'a >
type 'a replace = [`Replace of 'a | `Keep]

(* the current proposed interface for replacement. doesn't look pretty. *)
module type Replace = sig
  type t
  type str
  type group

  (* not sure whether to expose this one at all *)
  val replace_in_string : t -> str substr -> str replace -> str

  val replace_group : t -> f:(group -> t) -> str -> str
  val replace_match : t -> f:(str substr -> str) -> str
  val replace_all : t -> f:(str -> str) -> str -> str
end

module type Group = sig
  type t
  type str
  type index

  val group : t -> index -> str option
  val group_pos : t -> index -> (int * int) option

  val fold_left : t -> init:'a -> f:('a -> str substr -> 'a) -> 'a

  val all : t -> str list
  val alli : t -> (index * str) list

  val full_match : t -> str
  val full_match_pos : t -> int * int
end

module type Re = sig
  type t
  type str

  module Group : Group with type str = str

  val regexp : string -> t
  val quote : str -> t
  val matches : t -> str -> bool

  (* TODO add folding over splits? *)
  val split : ?max:int -> t -> str -> str list
  val split_delim : ?max:int -> t -> str -> [`Text of str | `Delim of str] list

  val fold_left_groups : t -> str -> init:'a -> f:('a -> Group.t -> 'a) ->'a
  val find_groups : t -> str -> Group.t list
  val find_concat_groups : t -> str -> str list

  val fold_left_match : t -> str -> init:'a -> f:('a -> str substr -> 'a) -> 'a
  val find_matches : t -> str -> string list

  (* TODO given up on replacement for now. will revisit once I'm content
     with an interface*)

  module Infix : sig
    val (=~) : str -> t -> bool
  end
end
