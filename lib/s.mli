(** Signatures Humane-re modules *)

(** A slice of a string *)
type 'a substr = < pos : int * int ; str : 'a >

module type Group = sig
  type t
  type str
  type index (** The type of the group name. Int usually. *)

  (** Extract the group at [index] if it exists. *)
  val group : t -> index -> str option
  (** Same as [group] but raises [Not_found] if group is not matched *)
  val group_exn : t -> index -> str
  (** Extracts the group position if it exists *)
  val group_pos : t -> index -> (int * int) option
  (** The most general group extraction. Returns the string and the position
      if it exists *)
  val group_substr : t -> index -> (str substr) option

  (** fold over the group matches in the match order *)
  val fold_left : t -> init:'a -> f:('a -> str substr -> 'a) -> 'a

  (** Return all matched strings in the order they were matched *)
  val all : t -> str list
  (** Return all matched strings with the group names that were matched. *)
  val alli : t -> (index * str) list

  (** Return the string encompassing the full match *)
  val full_match : t -> str
  (** Return the position encompassing the full match *)
  val full_match_pos : t -> int * int
end

module type Search = sig
  type re
  type str

  val forward : ?start:int -> re -> str -> str substr option
  (* val backward : ... *)
end

module type Re = sig
  type t (** The type of regular expression *)
  type str (** The type of string we match on *)

  module Group : Group with type str = str

  (** Compile a regular expression from a string *)
  val regexp : string -> t
  (** quote [str] returns a regular expression that matches [str] literally *)
  val quote : str -> t
  (** return true if the string matches the regular expression *)
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
