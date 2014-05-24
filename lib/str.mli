module Group : (S.Group with type index = int 
                         and type str = string)

include S.Re with type str = string
              and module Group := Group
