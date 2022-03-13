module PullVPA (
    tests
) where

import qualified Test

-- | Tree is a Tree of Characters.
-- | This means a list of trees, where all of the tress have no children, correspond to a string.
data Tree =
    Tree Char [Tree]
    deriving (Show)

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

data NodeExpr =
    NodeExpr Char Expr
    deriving (Show)

-- | deriveCall returns a list of all the Node expressions.
deriveCall :: Expr -> [NodeExpr]
deriveCall EmptySet = []
deriveCall Nil = []
deriveCall (Node a x) = [NodeExpr a x]
deriveCall (Or x y) = deriveCall x ++ deriveCall y
deriveCall (Concat x y) =
    if nullable x
    then deriveCall x ++ deriveCall y
    else deriveCall x
deriveCall (ZeroOrMore x) = deriveCall x

deriveCalls :: [Expr] -> [NodeExpr]
deriveCalls xs = concatMap deriveCall xs

-- | deriveReturn receives an expr and a list of matches.
-- | The matches correspond to whether the Node expressions matched the input children.
-- | Each node expression is replaced with a Nil or EmptySet based on whether that node matched.
-- | The matches list is read from the head and matches that are left are returned.
-- | This means that the original caller should end up with an empty list of matches.
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

-- | deriveReturns is a helper function for derives.
-- | deriveReturns calls deriveReturn for each expression, eating the concatenated list of matches from the head as it goes.
-- | deriveReturns terminates when it has executed deriveReturn for each expression and the list of matches is empty.
-- | deriveReturns :: exprs -> matches -> returned_exprs
deriveReturns :: [Expr] -> [Bool] -> [Expr]
deriveReturns [] [] = []
deriveReturns (x:xs) ms =
  let (x', ms') = deriveReturn x ms
  in x' : deriveReturns xs ms'

-- | derives is equivalent to the original derivative of a Tree in Tree.hs.
-- | It handles the matching of multiple NodeExprs and
-- | splits up the other operator logic between deriveCalls and deriveReturns.
-- | It is necessary that derives doesn't just derive one expression, but multiple together,
-- | because we are simulating a pull based parser, we can only look at a tree node once,
-- | but have to get the derivative of multiple child expressions.
derives :: [Expr] -> Tree -> [Expr]
derives xs (Tree a children) =
    let nodexs = deriveCalls xs in
    let childxs = map (\(NodeExpr b thn) -> if a == b then thn else EmptySet) nodexs in
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
vpaCall :: State -> Char -> (StackElm, State)
vpaCall xs a =
    let nodexs = deriveCalls xs in
    let xs' = map (\(NodeExpr b thn) -> if a == b then thn else EmptySet) nodexs in
    (xs, xs')

-- | vpaReturn takes the concatenated list of resulting expressions from the children and the original list of expressions.
-- | vpaReturn replaces all the Node expressions inside the list of expressions with:
-- | - a Nil expression if their corresponding resulting child expression is nullable or
-- | - an EmptySet expression if their corresponding resulting child expression is not nullable.
-- | vpaReturn ignores the input character, since it is already determined that vpaReturn will only be called when we are traversing back up the tree.
-- | vpaReturn :: state -> alphabet_return -> stack_elm -> state
vpaReturn :: State -> Char -> StackElm -> State
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
vpaDerive' state (Tree a children) =
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
    Test.test "match" match (Concat (Node 'a' Nil) (ZeroOrMore (Node 'b' Nil))) [Tree 'a' [], Tree 'b' [], Tree 'b' []] True
    Test.test "match" match (Concat (Node 'a' Nil) (ZeroOrMore (Node 'b' Nil))) [Tree 'a' [], Tree 'b' [], Tree 'c' []] False
    Test.test "match" match (Concat (Node 'a' (ZeroOrMore (Node 'b' Nil))) (Node 'c' (ZeroOrMore (Node 'd' Nil)))) [Tree 'a' [Tree 'b' [], Tree 'b' []], Tree 'c' []] True
    Test.test "match" match (Concat (Node 'a' (ZeroOrMore (Node 'b' Nil))) (Node 'c' (ZeroOrMore (Node 'd' Nil)))) [Tree 'a' [], Tree 'c' [Tree 'd' [], Tree 'b' []]] False
    Test.test "vpaMatch" vpaMatch (Concat (Node 'a' Nil) (ZeroOrMore (Node 'b' Nil))) [Tree 'a' [], Tree 'b' [], Tree 'b' []] True
    Test.test "vpaMatch" vpaMatch (Concat (Node 'a' Nil) (ZeroOrMore (Node 'b' Nil))) [Tree 'a' [], Tree 'b' [], Tree 'c' []] False
    Test.test "vpaMatch" vpaMatch (Concat (Node 'a' (ZeroOrMore (Node 'b' Nil))) (Node 'c' (ZeroOrMore (Node 'd' Nil)))) [Tree 'a' [Tree 'b' [], Tree 'b' []], Tree 'c' []] True
    Test.test "vpaMatch" vpaMatch (Concat (Node 'a' (ZeroOrMore (Node 'b' Nil))) (Node 'c' (ZeroOrMore (Node 'd' Nil)))) [Tree 'a' [], Tree 'c' [Tree 'd' [], Tree 'b' []]] False
