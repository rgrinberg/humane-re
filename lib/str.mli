module Group : (S.Group with type index = int
                         and type str = string)

include S.Re with type str = string
              and module Group := Group

module Search : (S.Search with type re = t and type str = string)
