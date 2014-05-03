module type S = (module type of S)

module Str : sig
  include S.Re
end
