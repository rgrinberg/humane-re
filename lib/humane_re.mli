module type Match = sig
  type t
  type str
  type index

  val get : t -> index -> str option
  val get_pos : t -> index -> (int * int) option

  val fold_left : t -> init:'a -> 
    f:('a -> pos:(int * int) -> str:str -> 'a) -> 'a

  val all_matched : t -> str list
  val all : t -> str option list
end

module type S = sig
  type t
  type str

  module Match : Match with type str = str

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



(* module String : (module S with str = string) *)
