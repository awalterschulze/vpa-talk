module PullVPAStr (
    tests,
    examples
) where

import qualified Test

import qualified Data.Tree as DataTree
import TreeConstructors
import qualified EgglessBananaBread
import qualified NuttyBananaBread
import qualified MapleBananaBread

type Tree = DataTree.Tree String

data Pred
    = Eq String
    | Leq Double
    | Any
    deriving (Eq, Ord, Show)

data Expr
    = EmptySet
    | Nil
    | Node Pred Expr
    | Or Expr Expr
    | And Expr Expr
    | Concat Expr Expr
    | ZeroOrMore Expr
    | Not Expr
    deriving (Eq, Ord, Show)

contains :: Expr -> Expr
contains x = Concat (Not EmptySet) (Concat x (Not EmptySet))

nullable :: Expr -> Bool
nullable EmptySet = False
nullable Nil = True
nullable (Node _ _) = False
nullable (Or x y) = nullable x || nullable y
nullable (And x y) = nullable x && nullable y
nullable (Concat x y) = nullable x && nullable y
nullable (ZeroOrMore _) = True
nullable (Not x) = not (nullable x)

data NodeExpr =
    NodeExpr Pred Expr
    deriving (Show)

-- | deriveCall returns a list of all the Node expressions.
deriveCall :: Expr -> [NodeExpr]
deriveCall EmptySet = []
deriveCall Nil = []
deriveCall (Node a x) = [NodeExpr a x]
deriveCall (Or x y) = deriveCall x ++ deriveCall y
deriveCall (And x y) = deriveCall x ++ deriveCall y
deriveCall (Concat x y) =
    if nullable x
    then deriveCall x ++ deriveCall y
    else deriveCall x
deriveCall (ZeroOrMore x) = deriveCall x
deriveCall (Not x) = deriveCall x

deriveCalls :: [Expr] -> [NodeExpr]
deriveCalls xs = concatMap deriveCall xs

-- | deriveReturn receives an expr and a list of matches.
-- | The matches correspond to whether the Node expressions matched the input children.
-- | Each node expression is replaced with a Nil or EmptySet based on whether that node matched.
-- | The matches list is read from the head and matches that are left are returned.
-- | This means that the original caller should end up with an empty list of matches.
-- | deriveReturn can be broken up into two functions:
-- |  - replaceNodeExprs, which replaces the nodeExprs with the derivative and
-- |  - deriveRest, which derives the rest of the expressions.
-- | deriveReturn :: expr -> matches -> (expr, [])
deriveReturn :: Expr -> [Bool] -> (Expr, [Bool])
deriveReturn EmptySet ms = (EmptySet, ms)
deriveReturn Nil ms = (EmptySet, ms)
deriveReturn (Node _ _) (match:ms) =
    if match
    then (Nil, ms)
    else (EmptySet, ms)
deriveReturn (Or x y) ms =
    let (x', ms') = deriveReturn x ms in
    let (y', ms'') = deriveReturn y ms' in
    (Or x' y', ms'')
deriveReturn (And x y) ms =
    let (x', ms') = deriveReturn x ms in
    let (y', ms'') = deriveReturn y ms' in
    (And x' y', ms'')
deriveReturn (Concat x y) ms =
    if nullable x
    then
        let (x', ms') = deriveReturn x ms in
        let (y', ms'') = deriveReturn y ms' in
        (Or (Concat x' y) y', ms'')
    else
        let (x', ms') = deriveReturn x ms in
        (Concat x' y, ms')
deriveReturn (ZeroOrMore x) ms =
    let (x', ms') = deriveReturn x ms in
    (Concat x' (ZeroOrMore x), ms')
deriveReturn (Not x) ms =
    let (x', ms') = deriveReturn x ms in
    (Not x', ms')

-- | deriveReturns is a helper function for derives.
-- | deriveReturns calls deriveReturn for each expression, eating the concatenated list of matches from the head as it goes.
-- | deriveReturns terminates when it has executed deriveReturn for each expression and the list of matches is empty.
-- | deriveReturns :: exprs -> matches -> returned_exprs
deriveReturns :: [Expr] -> [Bool] -> [Expr]
deriveReturns [] [] = []
deriveReturns (x:xs) ms =
  let (x', ms') = deriveReturn x ms
  in x' : deriveReturns xs ms'

evalPred :: Pred -> String -> Bool
evalPred (Eq x) y = x == y
evalPred (Leq x) y =
    (read y :: Double) <= x
evalPred Any _ = True

-- | derives is equivalent to the original derivative of a Tree in Tree.hs.
-- | It handles the matching of multiple NodeExprs and
-- | splits up the other operator logic between deriveCalls and deriveReturns.
-- | It is necessary that derives doesn't just derive one expression, but multiple together,
-- | because we are simulating a pull based parser, we can only look at a tree node once,
-- | but have to get the derivative of multiple child expressions.
derives :: [Expr] -> Tree -> [Expr]
derives xs (DataTree.Node a children) =
    let nodexs = deriveCalls xs in
    -- we have extracted some of  the logic that could have been part of deriveCalls,
    -- which could have return the child expressions, instead of the NodeExprs.
    -- then deriveCall's signature would have been:
    -- deriveCall :: Expr -> String -> Expr
    -- deriveCall (Node a x) b = if a == b then x else EmptySet
    let childxs = map (\(NodeExpr b thn) -> if evalPred b a then thn else EmptySet) nodexs in
    let ms = matches childxs children in
    let dxs = deriveReturns xs ms in
    dxs

matches :: [Expr] -> [Tree] -> [Bool]
matches xs ts = map nullable (foldl derives xs ts)

-- | match matches a hedge or list of trees against an expression.
match :: Expr -> [Tree] -> Bool
match x ts =
    let [m] = matches [x] ts
    in m

-- | VPA: Visibly PushDown Automaton
-- | a VPA has two operations:
-- | - Call:   State -> AlphabetCall -> (State, StackElm)
-- | - Return: State -> AlphabetReturn -> StackElm -> State
-- | The alphabets for call and return have to be disjoint.
-- | This means that it should be deterministic based on the next input character whether call or return is used.
-- | We should determine this without looking at the stack.
-- | For example, opening and closing parenthesis.
-- | Trees that have already been parsed, can also use Call when traversing down and Return when returning.
-- | The StackElm can simply use the call stack as we recurse down and return up the tree.

-- | State is a list of expressions to represent the list of Node expressions or child expressions that we send down the tree when recursing.
-- | The starting state is always a singleton list representing the starting expression.
type State = [Expr]

-- | StackElm also has to be a list of expressions as it represents the Node expressions' child expressions that were sent down when recursing.
-- | StackElm swaps places with State as call and return are used.
-- | - call: we save the State in the StackElem.
-- | - return: we load the derived StackElm as the State.
type StackElm = [Expr]

-- | vpaCall collects all the Node expressions' child expressions that it will send down to the children.
-- | vpaCall has to check if the input character matches the Node expression's expected character to determine whether:
-- | - it sends down the Node expression's child expression, when it matches or
-- | - it sends down the EmptySet, when it doesn't match.
-- | vpaCall also returns the input state as the stack element that we will need to remember until we traverse back up the tree.
-- | vpaCall :: state -> alphabet_call -> (state, stack_elm)
vpaCall :: State -> String -> (StackElm, State)
vpaCall xs a =
    let nodexs = deriveCalls xs in
    let xs' = map (\(NodeExpr b thn) -> if evalPred b a then thn else EmptySet) nodexs in
    (xs, xs')

-- | vpaReturn takes the concatenated list of resulting expressions from the children and the original list of expressions.
-- | vpaReturn replaces all the Node expressions inside the list of expressions with:
-- | - a Nil expression if their corresponding resulting child expression is nullable or
-- | - an EmptySet expression if their corresponding resulting child expression is not nullable.
-- | vpaReturn ignores the input character, since it is already determined that vpaReturn will only be called when we are traversing back up the tree.
-- | vpaReturn :: state -> alphabet_return -> stack_elm -> state
vpaReturn :: State -> String -> StackElm -> State
vpaReturn rs _ xs =
    let nulls = map nullable rs in
    let xs' = deriveReturns xs nulls in
    xs'

-- | vpaDerive' is a helper function for vpaDerive.
-- | vpaDerive' takes a list of expressions (State) instead of a single expression.
-- | vpaDerive' returns the derivative for the list of expressions given the tree.
-- | The list of expressions is necessary to facilitate recursing down,
-- | because we will need to take the derivative for each of the list of Node expressions' child expressions.
vpaDerive' :: State -> Tree -> State
vpaDerive' state (DataTree.Node a children) =
    let (stackElm, state') = vpaCall state a in
    let state'' = foldl vpaDerive' state' children in
    let state''' = vpaReturn state'' a stackElm in
    state'''

-- | vpaDerive is the equivalent of derive, simply using the vpaCall and vpaReturn functions,
-- | instead of the deriveCall and deriveReturn functions,
-- | such that vpaCall and vpaReturn correspond to the function signature expected of a VPA.
vpaDerive :: Expr -> Tree -> Expr
vpaDerive x t =
    let [x'] = vpaDerive' [x] t
    in x'

-- | vpaMatch is the equivalent of match, but using a VPA.
vpaMatch :: Expr -> [Tree] -> Bool
vpaMatch x s = nullable (foldl vpaDerive x s)

tests :: IO ()
tests = do
    Test.test "match" match (Concat (Node (Eq "a") Nil) (ZeroOrMore (Node (Eq "b") Nil))) [node "a" [], node "b" [], node "b" []] True
    Test.test "match" match (Concat (Node (Eq "a") Nil) (ZeroOrMore (Node (Eq "b") Nil))) [node "a" [], node "b" [], node "c" []] False
    Test.test "match" match (Concat (Node (Eq "a") (ZeroOrMore (Node (Eq "b") Nil))) (Node (Eq "c") (ZeroOrMore (Node (Eq "d") Nil)))) [node "a" [node "b" [], node "b" []], node "c" []] True
    Test.test "match" match (Concat (Node (Eq "a") (ZeroOrMore (Node (Eq "b") Nil))) (Node (Eq "c") (ZeroOrMore (Node (Eq "d") Nil)))) [node "a" [], node "c" [node "d" [], node "b" []]] False
    Test.test "vpaMatch" vpaMatch (Concat (Node (Eq "a") Nil) (ZeroOrMore (Node (Eq "b") Nil))) [node "a" [], node "b" [], node "b" []] True
    Test.test "vpaMatch" vpaMatch (Concat (Node (Eq "a") Nil) (ZeroOrMore (Node (Eq "b") Nil))) [node "a" [], node "b" [], node "c" []] False
    Test.test "vpaMatch" vpaMatch (Concat (Node (Eq "a") (ZeroOrMore (Node (Eq "b") Nil))) (Node (Eq "c") (ZeroOrMore (Node (Eq "d") Nil)))) [node "a" [node "b" [], node "b" []], node "c" []] True
    Test.test "vpaMatch" vpaMatch (Concat (Node (Eq "a") (ZeroOrMore (Node (Eq "b") Nil))) (Node (Eq "c") (ZeroOrMore (Node (Eq "d") Nil)))) [node "a" [], node "c" [node "d" [], node "b" []]] False


-- I have a vegan guest coming over:
-- .Ingredients:
--   !(
--     ._:.Name == "Eggs"
--    )

noeggs = Not (contains (Node Any (contains (Node (Eq "Name") (Node (Eq "Eggs") Nil)))))
example_vegan = contains (Node (Eq "Ingredients") noeggs)

-- .Ingredients:
--   ._:{
--     Name == "Bananas";
--     Number <= 3;
--     *
--   }

max_banans n = contains (Node Any (Concat (Not EmptySet) (Concat (Node (Eq "Name") (Node (Eq "Bananas") Nil)) (Concat (Node (Eq "Number") (Node (Leq n) Nil)) (Not EmptySet)))))
example_3bananas = contains (Node (Eq "Ingredients") (max_banans 3))

-- .Ingredients:
--   (
--     !(
--       ._:.Name == "Eggs"
--      ) &
--     ._:(
--        .Name == "Bananas" &
--        .Number <= 4
--     )
--   )

vegan_4bananas_example = contains (Node (Eq "Ingredients") (And noeggs (max_banans 4)))

examples :: IO ()
examples = do
    Test.test "match" match example_vegan EgglessBananaBread.example True
    Test.test "match" match example_3bananas EgglessBananaBread.example False
    Test.test "match" match vegan_4bananas_example EgglessBananaBread.example True
    Test.test "match" match example_vegan NuttyBananaBread.example False
    Test.test "match" match example_3bananas NuttyBananaBread.example False
    Test.test "match" match vegan_4bananas_example NuttyBananaBread.example False
    Test.test "match" match example_vegan MapleBananaBread.example False
    Test.test "match" match example_3bananas MapleBananaBread.example True
    Test.test "match" match vegan_4bananas_example MapleBananaBread.example False
