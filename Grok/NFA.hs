module NFA
-- special symbol
( epsilon
, eps
-- NFA types
, GNFA
, NFA
-- conversion from DFAs
, nondeterminiseDFA
-- well-formedness checking
, checkNFA
-- emulation
, acceptsNFA
-- accessor functions
, statesNFA
, alphabetNFA
, transnsNFA
, startStatesNFA
, acceptStatesNFA
-- NFA utilities
, trimNFA
, renameNFA
, renumberNFA
, reorderNFA
, removeEpsilonNFA
)
where


import Data.List (nub, (\\), sort)
import Data.Maybe (fromJust)

import DFA

-- ----------------------------
-- NFA types and special symbol
-- ----------------------------

epsilon :: Symbol
epsilon
  = 'ε'

-- for short:
eps :: Symbol
eps
  = epsilon

type GNFA a
  = ([a], [Symbol], [GTransn a], [a], [a])

type NFA
  = GNFA Int


-- | An DFA is just an NFA with a single start state (and no use of
-- nondeterministic transitions or epsilon transitions)
nondeterminiseDFA :: GDFA a -> GNFA a
nondeterminiseDFA (qs, xs, ts, q0, as)
  = (qs, xs, ts, [q0], as)

-- ---------------------------------
-- Well-formedness checking for NFAs
-- ---------------------------------

-- | Check an NFA for well-formedness
checkNFA :: Eq a => GNFA a -> CheckResult a
checkNFA nfa
  = allOK [ declaredStates nfa
          , declaredSymbols nfa
          , noEpsilon (alphabetNFA nfa)
          ]

declaredStates (qs, _, ts, q0s, as)
  | null undecl = OK
  | otherwise   = UndeclaredStates undecl
  where
    undecl = filter (not.(`elem` qs)) (q0s ++ inputQs ts ++ outputs ts ++ as)

declaredSymbols (_, xs, ts, _, _)
  | null undecl = OK
  | otherwise   = UndeclaredSymbols undecl
  where
    undecl = filter (not . (`elem` epsilon:xs)) (inputXs ts)

noEpsilon xs
  | not (epsilon `elem` xs) = OK
  | otherwise               = InvalidAlphabet [epsilon]


-- -------------
-- NFA emulation
-- -------------

-- | acceptsNFA nfa input
-- True iff `nfa` accepts when run on `input`.
acceptsNFA :: Eq a => GNFA a -> Input -> Bool
acceptsNFA nfa ys
  = any (`elem` as) qNs
  where
    (qs, xs, ts, q0s, as) = removeEpsilonNFA nfa
    qNs = foldl step q0s ys
    step qIs y
      = nub $ concat [lookupAll (q, y) ts | q <- qIs]


-- -------------------------------
-- Functions for working with NFAs
-- -------------------------------

-- | The list of states from the nfa
statesNFA :: GNFA a -> [a]
statesNFA (qs, _, _, _, _)
  = qs

-- | The list of symbols / the alphabet from the nfa
alphabetNFA :: GNFA a -> [Symbol]
alphabetNFA (_, xs, _, _, _)
  = xs

-- | The list of transitions from the nfa
transnsNFA :: GNFA a -> [GTransn a]
transnsNFA (_, _, ts, _, _)
  = ts

-- | The list of start states of the nfa
startStatesNFA :: GNFA a -> [a]
startStatesNFA (_, _, _, q0s, _)
  = q0s

-- | The list of accept states from the nfa
acceptStatesNFA :: GNFA a -> [a]
acceptStatesNFA (_, _, _, _, as)
  = as



-- ----------------------------------
-- Helper functions for altering NFAs
-- ----------------------------------

-- | Alter nfa by removing unreachable states and non-start states from which
-- no accept state is reachable. For OK NFAs, this returns an equivalent NFA.
trimNFA :: Eq a => GNFA a -> GNFA a
trimNFA (qs, xs, ts, q0s, as)
  = (qs', xs, ts', q0s', as')
  where
    -- extract transition graph
    g = edges ts

    -- filters for reachable and co-reachable states
    accessible q
      = q `elem` (concatMap (reachables g) q0s)
    acceptable q
      = any (`elem` as) (reachables g q)
    
    -- keep only accessible and acceptable states
    q0s' = filter acceptable q0s
    as'  = filter accessible as
    qs'  = filter (\q -> accessible q && acceptable q) qs
    ts'  = filter (\((q,_),r) -> elem q qs' && elem r qs') ts



-- | Change the name of each state throughout the NFA's definition,
-- according to a renaming function `name`.
--
-- NOTE: `name` should be an injective function on the set of states of the
-- NFA, otherwise some states will have their names conflated.
renameNFA :: (a -> b) -> GNFA a -> GNFA b
renameNFA name (qs, xs, ts, q0s, as)
  = (qs', xs, ts', q0s', as')
  where
    qs' = map name qs
    ts' = [((name q, x), name r) | ((q, x), r) <- ts]
    q0s'= map name q0s
    as' = map name as

-- | Rename all states of `nfa` with unique integers starting from 0
renumberNFA :: (Eq a, Innovative a) => GNFA a -> NFA
renumberNFA nfa
  = renameNFA (fromJust . (`lookup` names)) nfa
  where
    names = zip (statesNFA nfa) [0..]

-- | Reorder all states and symbols of `nfa` so that they occur in
-- sorted order, including in the transition function
reorderNFA :: Ord a => GNFA a -> GNFA a
reorderNFA (qs, xs, ts, q0, as)
  = (sort qs, sort xs, sort ts, sort q0, sort as)


-- | Returns an equivalent NFA with any epsilon transitions replaced
-- by transitions on real alphabet symbols. For OK NFAs this returns an
-- equivalent OK NFA.
removeEpsilonNFA :: Eq a => GNFA a -> GNFA a
removeEpsilonNFA (qs, xs, ts, q0s, as)
  = (qs, xs, ts', q0s, as')
  where
    gEps = edgesOn epsilon ts

    -- accept from anywhere with epsilon-reachable accept states
    acceptable q = any (`elem` as) (reachables gEps q)
    as' = filter acceptable qs

    -- transition to anywhere with an epsilon-reachable transition there
    ts' = [ ((q, x), s)
          | q <- qs
          , r <- nub (reachables gEps q)
          , (x, s) <- nub [(x, s) | ((r',x),s) <- ts, x /= epsilon, r' == r]
          ]
