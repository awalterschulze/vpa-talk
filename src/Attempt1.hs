derive (Symbol b) a = if a == b then EmptyStr else EmptySet

derive (Node b x) (Tree a children) =
    if a == b && match x children
    then Nil
    else EmptySet

deriveReturn (Node _ _) (match:ms) =
    if match
    then (Nil, ms)
    else (EmptySet, ms)