type 'a substr = < pos : int * int ; str : 'a >

module type Group = sig
  type t
  type str
  type index

  val group : t -> index -> str option
  val group_pos : t -> index -> (int * int) option

  val fold_left : t -> init:'a -> 
    f:('a -> str substr -> 'a) -> 'a

  val all : t -> str list
  val alli : t -> (index * string) list

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

  val split : ?max:int -> t -> str -> str list
  val split_delim : ?max:int -> t -> str -> [`Text of str | `Delim of str] list

  val fold_left_groups : t -> str -> init:'a -> f:('a -> Group.t -> 'a) ->'a
  val find_groups : t -> str -> Group.t list
  val find_concat_groups : t -> str -> str list

  val fold_left_match : t -> str -> init:'a -> f:('a -> str substr -> 'a) -> 'a
  val find_matches : t -> str -> string list

  val replace_all_group : t ->
    f:(Group.t -> [`Replace of str | `Keep ] list) -> str -> str
  val replace_all : t -> f:(str -> str) -> str -> str

  module Infix : sig
    val (=~) : str -> t -> bool
  end
end
