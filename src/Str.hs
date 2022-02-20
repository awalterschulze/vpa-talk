module Str (
    tests
) where

import qualified Test

data Expr
    = EmptySet
    | EmptyStr
    | Symbol Char
    | Or Expr Expr
    | Concat Expr Expr
    | ZeroOrMore Expr
    deriving (Eq, Ord, Show)

nullable :: Expr -> Bool
nullable EmptySet = False
nullable EmptyStr = True
nullable (Symbol _) = False
nullable (Or x y) = nullable x || nullable y
nullable (Concat x y) = nullable x && nullable y
nullable (ZeroOrMore _) = True

derive :: Expr -> Char -> Expr
derive EmptySet _ = EmptySet
derive EmptyStr _ = EmptySet
derive (Symbol b) a = if a == b then EmptyStr else EmptySet
derive (Or x y) a = Or (derive x a) (derive y a)
derive (Concat x y) a =
    if nullable x
    then Or (Concat (derive x a) y) (derive y a)
    else Concat (derive x a) y
derive (ZeroOrMore x) a = Concat (derive x a) (ZeroOrMore x)

match :: Expr -> String -> Bool
match x s = nullable (foldl derive x s)

tests :: IO ()
tests = do
    Test.test "match" match (Concat (Symbol 'a') (ZeroOrMore (Symbol 'b'))) "abb" True
    Test.test "match" match (Concat (Symbol 'a') (ZeroOrMore (Symbol 'b'))) "abc" False
