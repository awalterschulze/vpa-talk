module Tree (
    tests
) where

import qualified Test

data Tree = Tree Char [Tree] deriving (Show)

data Expr
    = EmptySet
    | Nil
    | Node Char Expr
    | Or Expr Expr
    | Concat Expr Expr
    | ZeroOrMore Expr
    deriving (Eq, Ord, Show)

nullable :: Expr -> Bool
nullable EmptySet = False
nullable Nil = True
nullable (Node _ _) = False
nullable (Or x y) = nullable x || nullable y
nullable (Concat x y) = nullable x && nullable y
nullable (ZeroOrMore _) = True

derive :: Expr -> Tree -> Expr
derive EmptySet _ = EmptySet
derive Nil _ = EmptySet
derive (Node b x) (Tree a children) =
    if a == b && match x children
    then Nil
    else EmptySet
derive (Or x y) a = Or (derive x a) (derive y a)
derive (Concat x y) a =
    if nullable x
    then Or (Concat (derive x a) y) (derive y a)
    else Concat (derive x a) y
derive (ZeroOrMore x) a = Concat (derive x a) (ZeroOrMore x)

-- | match matches a hedge or list of trees against an expression.
match :: Expr -> [Tree] -> Bool
match x s = nullable (foldl derive x s)

tests :: IO ()
tests = do
    Test.test "match" match (Concat (Node 'a' Nil) (ZeroOrMore (Node 'b' Nil))) [Tree 'a' [], Tree 'b' [], Tree 'b' []] True
    Test.test "match" match (Concat (Node 'a' Nil) (ZeroOrMore (Node 'b' Nil))) [Tree 'a' [], Tree 'b' [], Tree 'c' []] False
    Test.test "match" match (Concat (Node 'a' (ZeroOrMore (Node 'b' Nil))) (Node 'c' (ZeroOrMore (Node 'd' Nil)))) [Tree 'a' [Tree 'b' [], Tree 'b' []], Tree 'c' []] True
    Test.test "match" match (Concat (Node 'a' (ZeroOrMore (Node 'b' Nil))) (Node 'c' (ZeroOrMore (Node 'd' Nil)))) [Tree 'a' [], Tree 'c' [Tree 'd' [], Tree 'b' []]] False
