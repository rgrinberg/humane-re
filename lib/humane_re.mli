module type S = sig
  type t
  type str

  module Match : sig
    type t
    type kind = [`Index of int | `Name of string]
    type pos = int * int
    val get : t -> kind -> string option
    val get_pos : t -> kind -> pos option
    val get_all : t -> (kind * string) list
  end

  val regexp : string -> t
  val quote : str -> t
  val matches : t -> str -> bool

  val split : ?max:int -> t -> str -> str list
  val split_delim : ?max:int -> t -> str -> [`Text of str | `Delim of str]

  val find_matches : t -> str -> Match.t list
  val find_all : t -> str -> string list

  val replace_all : t -> str -> f:(Match.t -> str) -> t -> str

  module Infix : sig
    val (=~) : str -> t -> bool
  end
end

module String : S with str = string
