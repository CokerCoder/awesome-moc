module DFA
-- symbols and input types
( Symbol
, Input
-- general types for general states
, GTransn
, GDFA
, Innovative(innovate)
-- basic types with integer states
, Transn
, DFA
-- well-formedness checking
, CheckResult(..)
, allOK
, checkDFA
, checkCompleteDFA
-- emulation
, acceptsDFA
-- accessor functions
, statesDFA
, alphabetDFA
, transnsDFA
, startStateDFA
, acceptStatesDFA
-- working with transition functions
-- and reachability
, inputs
, outputs
, inputQs
, inputXs
, edges
, edgesOn
, successors
, reachables
, path
-- list helpers
, crosslist
, powerlist
, lookupAll
-- DFA utilities
, trimDFA
, completeDFA
, renameDFA
, renumberDFA
, reorderDFA
)
where


import Data.List (nub, (\\), sort, intercalate, nubBy, deleteFirstsBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Function (on)

-- ---------------------
-- DFA types and classes
-- ---------------------

type Symbol
  = Char

type Input
  = [Symbol]

type GTransn a
  = ((a, Char), a)

type GDFA a
  = ([a], [Symbol], [GTransn a], a, [a])


-- --------------------------------------------------------------
-- For simplicity, we will often want to work with integer states
-- --------------------------------------------------------------

type Transn
  = GTransn Int

type DFA
  = GDFA Int


-- -------------------------------------------------
-- Type classes (you can safely ignore this section)
-- -------------------------------------------------

-- Many DFA and NFA algorithms require us to be able to invent new
-- states with unique labels. Innovative is a typeclass that captures
-- this functionality.
class Innovative a where
  innovate :: [a] -> a

instance Innovative Int where
  innovate qs
    = 1 + maximum qs

instance Innovative Char where
  innovate qs
    = succ $ maximum qs

instance (Innovative a, Innovative b) => Innovative (a, b) where
  innovate qrs
    = (innovate (map fst qrs), innovate (map snd qrs))

instance Innovative a => Innovative [a] where
  innovate qss
    = [innovate (concat qss)]


-- ---------------------------------
-- Well-formedness checking for DFAs
-- ---------------------------------

data CheckResult a
  = OK
  | UndeclaredStates [a]
  | UndeclaredSymbols [Symbol]
  | Nondeterministic [(a, Symbol)]
  | Incomplete [(a, Symbol)]
  | InvalidAlphabet [Symbol]
  | MultipleProblems [CheckResult a]
  deriving Eq

instance Show a => Show (CheckResult a) where
  show OK
    = "OK"
  show (UndeclaredStates qs)
    = "Error: The following states occur in the automaton, but not declared"
      ++ " in the state list: " ++ show qs
  show (UndeclaredSymbols xs)
    = "Error: The following symbols occur in the automaton, but not declared"
      ++ " in the alphabet: " ++ intercalate ", " (map (:"") xs)
  show (Nondeterministic ts)
    = "Error: The following transition inputs occur with multiple outputs"
      ++ " in the transition function: " ++ show ts
  show (Incomplete ts)
    = "Error: The following transition inputs are missing, so the machine is"
      ++ " not completely specified: " ++ show ts
  show (InvalidAlphabet xs)
    = "Error: The following symbols are not allowed in the alphabet: "
      ++ intercalate ", " (map (:"") xs)
  show (MultipleProblems crs)
    = intercalate "\n" ("Multiple Errors:":map show crs)

-- | Agregate a list of checkresults
allOK :: Eq a => [CheckResult a] -> CheckResult a
allOK crs
  | null notOK        = OK
  | length notOK == 1 = head notOK
  | otherwise         = MultipleProblems notOK
  where
    notOK = filter (/=OK) crs


-- | Check a DFA for well-formedness
checkDFA :: Eq a => GDFA a -> CheckResult a
checkDFA dfa
  = allOK [ declaredStates dfa
          , declaredSymbols dfa
          , deterministic (transnsDFA dfa)
          ]

checkCompleteDFA :: (Eq a, Innovative a) => GDFA a -> CheckResult a
checkCompleteDFA (qs, xs, ts, _, _)
  | null missing = OK
  | otherwise    = Incomplete missing
  where
    missing = (crosslist qs xs) \\ (inputs ts)

declaredStates (qs, _, ts, q0, as)
  | null undecl = OK
  | otherwise   = UndeclaredStates undecl
  where
    undecl = filter (not . (`elem` qs)) (q0 : inputQs ts ++ outputs ts ++ as)

declaredSymbols (_, xs, ts, _, _)
  | null undecl = OK
  | otherwise   = UndeclaredSymbols undecl
  where
    undecl = filter (not . (`elem` xs)) (inputXs ts)

deterministic ts
  | null dups = OK
  | otherwise = Nondeterministic dups
  where
    inps = inputs ts
    dups = nub (inps \\ (nub inps))




-- ---------------------
-- DFA and NFA emulation
-- ---------------------

-- | acceptsDFA dfa input
-- True iff `dfa` accepts when run on `input`.
acceptsDFA :: (Eq a, Innovative a) => GDFA a -> Input -> Bool
acceptsDFA (qs, xs, ts, q0, as) ys
  = qN `elem` as
  where
    qN = foldl step q0 ys
    step q y
      = fromMaybe qX (lookup (q, y) ts)
    qX = innovate qs


-- -------------------------------
-- Functions for working with DFAs
-- -------------------------------

-- | The list of states from the dfa
statesDFA :: GDFA a -> [a]
statesDFA (qs, _, _, _, _)
  = qs

-- | The list of symbols / the alphabet from the dfa
alphabetDFA :: GDFA a -> [Symbol]
alphabetDFA (_, xs, _, _, _)
  = xs

-- | The list of transitions from the dfa
transnsDFA :: GDFA a -> [GTransn a]
transnsDFA (_, _, ts, _, _)
  = ts

-- | The start state of the dfa
startStateDFA :: GDFA a -> a
startStateDFA (_, _, _, q0, _)
  = q0

-- | The list of accept states from the dfa
acceptStatesDFA :: GDFA a -> [a]
acceptStatesDFA (_, _, _, _, as)
  = as


-- --------------------------------------------------------------------
-- Helper functions for working with transition functions and relations
-- --------------------------------------------------------------------

-- | inputs ts
-- The inputs from the transitions in `ts`
-- (the source state, transition symbol pairs)
inputs :: [GTransn a] -> [(a, Symbol)]
inputs ts
  = map fst ts

-- | inputQs ts
-- The state parts of the inputs from the transitions in `ts`
-- (the source states of the transitions)
inputQs :: [GTransn a] -> [a]
inputQs ts
  = map fst (inputs ts)

-- | inputXs ts
-- The symbol parts of the inputs from the transitions in `ts`
-- (the transition symbols)
inputXs :: [GTransn a] -> [Symbol]
inputXs ts
  = map snd (inputs ts)

-- | outputs ts
-- The outputs of the transitions in `ts`
-- (the destination states of the transitions)
outputs :: [GTransn a] -> [a]
outputs ts
  = map snd ts


-- | edges ts
-- The input states and output states of the transitions in `ts`
-- (the source state, destination state pairs)
edges :: [GTransn a] -> [(a, a)]
edges ts
  = zip (inputQs ts) (outputs ts)

-- | edgesOn x ts
-- The source/destination state pairs for all transitions in `ts`
-- that involve the symbol `x`
edgesOn :: Symbol -> [GTransn a] -> [(a, a)]
edgesOn x ts
  = [(q, r) | ((q, x'), r) <- ts, x' == x]


-- ----------------------------------------
-- Helper functions concerning reachability
-- ----------------------------------------

-- | successors g u
-- Where `g` is a list of edges (as from `edges ts` or `edgesOn x ts`),
-- and `u` is a state, return a list of those states which can be transitioned
-- to from `u` in one step
--
-- >>> successors [(1,2),(1,3),(2,4)] 1
-- [2,3]
-- >>> successors [(1,2),(1,3),(2,4)] 2
-- [4]
successors :: Eq a => [(a, a)] -> a -> [a]
successors g u
  = nub (lookupAll u g)


-- | reachables g u
-- Where `g` is a list of edges (as from `edges ts` or `edgesOn x ts`),
-- and `u` is a state, return a list of those states which can be transitioned
-- to from `u` in one or more steps (and zero steps, i.e., `u` itself)
-- 
-- >>> reachables [(1,2),(1,3),(2,4),(5,6)] 1
-- [1,2,3,4]
reachables :: Eq a => [(a, a)] -> a -> [a]
reachables g u
  = map snd (spantree g u)

-- | path g u v
-- Just a path from u to v in g, or Nothing if not reachable
-- 
-- >>> path [(1,2),(1,3),(2,4),(5,6)] 1 4
-- Just [1,2,4]
-- >>> path [(1,2),(1,3),(2,4),(5,6)] 2 1
-- Nothing
-- >>> path [(1,2),(1,3),(2,4),(5,6)] 1 5
-- Nothing
path :: Eq a => [(a, a)] -> a -> a -> Maybe [a]
path g u v
  | v `elem` reach = Just (reverse (reconstruct u v))
  | otherwise      = Nothing
  where
    tree  = spantree g u
    reach = map snd tree
    reconstruct u' v'
      | u' == v'  = [v']
      | otherwise = v' : reconstruct u' (head [x | (x, y) <- tree, y == v'])


-- | spantree g u
-- A spanning tree of u's connected component in g
--
-- >>> spantree [(1,2),(2,1),(2,3),(1,3),(4,5),(3,4),(6,5)] 1
-- [(1,1),(1,2),(1,3),(3,4),(4,5)]
-- >>> spantree [(1,2),(2,1),(2,3),(3,1),(2,1),(1,3),(4,3),(4,5),(3,4)] 1
-- [(1,1),(1,2),(1,3),(3,4),(4,5)]
-- >>> spantree [(1,2),(1,3),(1,4),(2,1),(3,1),(4,1)] 1
-- [(1,1),(1,2),(1,3),(1,4)]
-- >>> spantree [(1,2),(1,3),(1,4),(2,2),(2,3),(2,4),(3,2),(3,4),(4,3)] 1
-- [(1,1),(1,2),(1,3),(1,4)]
spantree :: Eq a => [(a, a)] -> a -> [(a, a)]
spantree g u
  = fst $ until (null.snd) expand ([], [(u, u)])
  where
    expand (tree, frontier)
      = let tree' = tree ++ frontier in 
      ( tree'
        , deleteFirstsBy eqSnd 
            (nubBy eqSnd [(e, f) | (_, e) <- frontier, f <- successors g e])
            tree'
        )
    eqSnd = (==) `on` snd


-- ---------------------------------------
-- Helper functions for working with lists
-- ---------------------------------------

-- | crosslist xs ys
-- Cartesian product of `xs` and `ys`: A list of all combinations
-- of one item from `xs` and another from `ys`, as (x, y) pairs
crosslist :: [a] -> [b] -> [(a, b)]
crosslist xs ys
  = [(x, y) | x <- xs, y <- ys]

-- | powerlist xs
-- A list of all possible sublists of `xs`.
--
-- >>> powerlist []
-- [[]]
-- >>> powerlist "0"
-- ["","0"]
-- >>> powerlist "01"
-- ["","0","1","01"]
powerlist :: [a] -> [[a]]
powerlist []
  = [[]]
powerlist (x:xs)
  = concatMap (\xs -> [xs, x:xs]) (powerlist xs)


-- | lookupAll xys x
-- Lookup all corresponding elements in an association list
--
-- >>> lookupAll 1 [(1,2),(1,3),(1,2),(2,1)]
-- [2,3,2]
-- >>> lookupAll 3 [(1,2),(1,3),(1,2),(2,1)]
-- []
lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll x' xys
  = [y | (x, y) <- xys, x == x']


-- ----------------------------------
-- Helper functions for altering DFAs
-- ----------------------------------

-- | Alter DFA by removing unreachable states and non-start states from which
-- no accept state is reachable. For OK DFAs, this returns an equivalent DFA.
trimDFA :: Eq a => GDFA a -> GDFA a
trimDFA (qs, xs, ts, q0, as)
  = (qs', xs, ts', q0, as')
  where
    -- extract transition graph
    g = edges ts
    -- discard unreachable accept states
    accessible q
      = q `elem` (reachables g q0)
    as' = filter accessible as
    -- discard states (other than q0) unable to reach feasible accept states
    acceptable q
      = (q == q0) || (any (`elem` as') (reachables g q))
    qs' = filter (\q -> accessible q && acceptable q) qs
    -- discard now-irrelevant transitions
    ts' = filter (\((q,_),r) -> q `elem` qs' && r `elem` qs') ts


-- | Alter DFA's transition function by making any missing transitions
-- explicit, adding an extra 'dead state' if necessary.
-- For OK DFAs, this returns an equivalent DFA.
completeDFA :: (Eq a, Innovative a) => GDFA a -> GDFA a
completeDFA (qs, xs, ts, q0, as)
  = (qs', xs, ts', q0, as)
  where
    missed = crosslist qs xs \\ inputs ts
    qX  = innovate qs
    qXs = if null missed then [] else [qX]
    qs' = qs ++ qXs
    ts' = ts ++ zip missed (repeat qX) ++ [((q, x), q) | q <- qXs, x <- xs]


-- | Change the name of each state throughout the DFA's definition,
-- according to a renaming function `name`.
--
-- WARNING:
-- `name` should be an injective function on the set of states of the
-- DFA, otherwise some states will have their names conflated.
renameDFA :: (a -> b) -> GDFA a -> GDFA b
renameDFA name (qs, xs, ts, q0, as)
  = (qs', xs, ts', q0', as')
  where
    qs' = map name qs
    ts' = [((name q, x), name r) | ((q, x), r) <- ts]
    q0' = name q0
    as' = map name as

-- | Rename all states of `dfa` with unique integers starting from 0
renumberDFA :: (Eq a, Innovative a) => GDFA a -> DFA
renumberDFA dfa
  = renameDFA (fromJust . (`lookup` names)) dfa
  where
    names = zip (statesDFA dfa) [0..]

-- | Reorder all states and symbols of `dfa` so that they occur in
-- sorted order, including in the transition function
reorderDFA :: Ord a => GDFA a -> GDFA a
reorderDFA (qs, xs, ts, q0, as)
  = (sort qs, sort xs, sort ts, q0, sort as)

