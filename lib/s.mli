module type Group = sig
  type t
  type index

  val group : t -> index -> string option
  val group_pos : t -> index -> (int * int) option

  val fold_left : t -> init:'a -> f:('a -> string Lazy.t -> int * int -> 'a) -> 'a

  val all : t -> string list
  val alli : t -> (index * string) list

  val full_match : t -> string
  val full_match_pos : t -> int * int

  val map : t -> f:(string Lazy.t -> int * int -> string) -> t
end

module type Re = sig
  type t

  module Group : Group

  val regexp : string -> t
  val quote : string -> t
  val matches : t -> string -> bool

  val split : ?max:int -> t -> string -> string list
  val split_delim : ?max:int -> t -> string -> [`Text of string | `Delim of string] list

  val fold_left_groups : t -> string -> init:'a -> f:('a -> Group.t -> 'a) ->'a
  val find_groups : t -> string -> Group.t list
  val find_concat_groups : t -> string -> string list

  val fold_left_match : t -> string -> init:'a -> f:('a -> string substr -> 'a) -> 'a
  val find_matches : t -> string -> string list

  val replace_all_group : t ->
    f:(Group.t -> Group.t) -> string -> string
  val replace_all : t -> f:(string -> string) -> string -> string

  module Infix : sig
    val (=~) : string -> t -> bool
  end
end
