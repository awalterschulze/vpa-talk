module StrVPA (
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

data SymbolExpr = SymbolExpr Char

nullable :: Expr -> Bool
nullable EmptySet = False
nullable EmptyStr = True
nullable (Symbol _) = False
nullable (Or x y) = nullable x || nullable y
nullable (Concat x y) = nullable x && nullable y
nullable (ZeroOrMore _) = True

-- | deriveCall returns a list of all the Symbol expressions.
deriveCall :: Expr -> [SymbolExpr]
deriveCall EmptySet = []
deriveCall EmptyStr = []
deriveCall (Symbol a) = [SymbolExpr a]
deriveCall (Or x y) = deriveCall x ++ deriveCall y
deriveCall (Concat x y) =
    if nullable x
    then deriveCall x ++ deriveCall y
    else deriveCall x
deriveCall (ZeroOrMore x) = deriveCall x

-- | deriveReturn receives an expr and a list of matches.
-- | The matches correspond to whether the Symbol expressions matched the input character.
-- | Each Symbol expression is replaced with an EmptyStr or EmptySet based on whether that Symbol matched.
-- | The matches list is read from the head and matches that are left are returned.
-- | This means that the original caller should end up with an empty list of matches.
-- | deriveReturn :: expr -> matches -> (expr, [])
deriveReturn :: Expr -> [Bool] -> (Expr, [Bool])
deriveReturn EmptySet ms = (EmptySet, ms)
deriveReturn EmptyStr ms = (EmptySet, ms)
deriveReturn (Symbol _) (match:ms) =
    if match
    then (EmptyStr, ms)
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

-- | derive is equivalent to the original derivative of a Char in Str.hs.
-- | It handles the matching of the SymbolExpr itself and
-- | splits up the other operator logic between deriveCall and deriveReturn.
derive :: Expr -> Char -> Expr
derive x a =
    let symbolExprs = deriveCall x in
    let matches = map (\(SymbolExpr b) -> b == a) symbolExprs in
    let (x', []) = deriveReturn x matches in
    x'

match :: Expr -> String -> Bool
match x s = nullable (foldl derive x s)

-- | VPA: Visibly PushDown Automaton
-- | a VPA has two operations:
-- | - Call:   State -> AlphabetCall -> (State, StackElm)
-- | - Return: State -> AlphabetReturn -> StackElm -> State
-- | The alphabets for call and return have to be disjoint.
-- | This means that it should be deterministic based on the next input character whether call or return is used.
-- | We should determine this without looking at the stack.
-- | For example, opening and closing parenthesis.
-- | Trees that have already been parsed, can also use Call when traversing down and Return when returning.

-- | For the string case, we never need a stack larger than one, so we simply use a variable.
-- | We also see each character as an open and closing parenthesis, For example "ab" => "<a></a><b></b>".

-- | State is a list of expressions to represent the list of derived Symbol expressions.
-- | The starting state is always a singleton list representing the starting expression.
type State = [Expr]

-- | StackElm is the original single expression that we use to replace the derived symbol expressions in as we calculate the derivative.
-- | StackElm swaps places with State as call and return are used.
-- | - call: before we call, we save the State in the StackElem, and at this state the state will only have one expression.
-- | - return: after we retrun, we load the derived StackElm as the State.
type StackElm = Expr

-- | vpaCall collects all the Symbol expressions and checks if the input character matches the Symbol expression's expected character to determine the resulting derivative expression: EmptyStr or EmptySet.
-- | vpaCall also returns the input state as the stack element that we will need to remember until for vpaReturn.
-- | vpaCall :: state -> alphabet_call -> (state, stack_elm)
vpaCall :: State -> Char -> (StackElm, State)
vpaCall [x] a =
    let symbolExprs = deriveCall x in
    let xs' = map (\(SymbolExpr b) -> if a == b then EmptyStr else EmptySet) symbolExprs in
    (x, xs')

-- | vpaReturn takes the list of Symbol expressions and replaces all the Symbol expressions inside the list of expressions with:
-- | - a EmptyStr expression if they matched the input character or
-- | - an EmptySet expression if they didn't matched the input character.
-- | vpaReturn ignores and doesn't actually require the input character, but we keep it here to have the proper vpaReturn signature.
-- | vpaReturn :: state -> alphabet_return -> stack_elm -> state
vpaReturn :: State -> Char -> StackElm -> State
vpaReturn rs _ x =
    let nulls = map nullable rs in
    let (x', []) = deriveReturn x nulls in
    [x']

-- | vpaDerive' is a helper function for vpaDerive.
-- | vpaDerive' takes a list of expressions (State) instead of a single expression.
-- | vpaDerive' returns the derivative for the list of expressions given a character.
-- | The list of expressions is necessary to facilitate extracting list of Symbol expressions and then replacing them,
-- | because we will need to take the derivative for each of the list of Symbol expressions.
vpaDerive' :: State -> Char -> State
vpaDerive' state a =
    let (stackElm, state') = vpaCall state a in
    let state'' = vpaReturn state' a stackElm in
    state''

-- | vpaDerive is the equivalent of derive, simply using the vpaCall and vpaReturn functions,
-- | instead of the deriveCall and deriveReturn functions,
-- | such that vpaCall and vpaReturn correspond to the function signature expected of a VPA.
vpaDerive :: Expr -> Char -> Expr
vpaDerive x t =
    let [x'] = vpaDerive' [x] t
    in x'

-- | vpaMatch is the equivalent of match, but using a VPA.
vpaMatch :: Expr -> [Char] -> Bool
vpaMatch x s = nullable (foldl vpaDerive x s)

tests :: IO ()
tests = do
    Test.test "match" match (Concat (Symbol 'a') (ZeroOrMore (Symbol 'b'))) "abb" True
    Test.test "match" match (Concat (Symbol 'a') (ZeroOrMore (Symbol 'b'))) "abc" False
    Test.test "vpaMatch" match (Concat (Symbol 'a') (ZeroOrMore (Symbol 'b'))) "abb" True
    Test.test "vpaMatch" match (Concat (Symbol 'a') (ZeroOrMore (Symbol 'b'))) "abc" False
