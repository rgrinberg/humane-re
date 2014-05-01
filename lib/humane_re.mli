module type S = sig
  type t
  type str

  module Match : sig
    type t
    type index
    type pos = int * int

    val get : t -> index -> string option
    val get_pos : t -> index -> pos option

    val all_matched : t -> string list
    val all : t -> string option list
  end

  val regexp : string -> t
  val quote : str -> t
  val matches : t -> str -> bool

  val split : ?max:int -> t -> str -> str list
  val split_delim : ?max:int -> t -> str -> [`Text of str | `Delim of str]

  val fold_left_matches : t -> str 
    -> init:'a -> f:('a -> Match.t -> 'a) -> 'a
  val find_matches : t -> str -> Match.t list
  val find_all : t -> str -> string list

  val replace_all : t -> str -> f:(Match.t -> str) -> t -> str

  module Infix : sig
    val (=~) : str -> t -> bool
  end
end

(* module String : (module S with str = string) *)
