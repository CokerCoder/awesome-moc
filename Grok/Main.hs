import DFA
import Data.List
import Data.Maybe
import NFA
import VisDFA

main :: IO ()
main = return ()

complementDFA :: DFA -> DFA
complementDFA dfa =
  (qs, xs, ts, q0, qs \\ as)
  where
    (qs, xs, ts, q0, as) = completeDFA dfa

productDFA :: DFA -> DFA -> DFA
productDFA (qs, xs, ts, q0, as) (qs', xs', ts', q0', as') =
  renumberDFA $ trimDFA (qs'', xs'', ts'', q0'', as'')
  where
    qs'' = crosslist qs qs'
    xs'' = union xs xs'
    q0'' = (q0, q0')
    as'' = crosslist as as'

    ts'' =
      [ (((q, q'), x), step (q, q') x)
        | (q, q') <- qs'',
          x <- intersect xs xs',
          (q, x) `elem` inputs ts,
          (q', x) `elem` inputs ts'
      ]

    step :: (Int, Int) -> Symbol -> (Int, Int)
    step (q, q') x =
      (fromJust (lookup (q, x) ts), fromJust (lookup (q', x) ts'))

determiniseNFA :: NFA -> DFA
determiniseNFA nfa =
  renumberDFA $ trimDFA (qs', xs, ts', q0', as')
  where
    (qs, xs, ts, q0s, as) = reorderNFA $ removeEpsilonNFA nfa

    q0' = q0s
    qs' = powerlist qs
    as' = filter (any (`elem` as)) qs'
    ts' = [((rs, x), step rs x) | rs <- qs', x <- xs]

    step :: [Int] -> Symbol -> [Int]
    step rs x =
      nub $ sort $ concatMap (`step1` x) rs

    -- TODO: Complete definition for step1 r x
    step1 :: Int -> Symbol -> [Int]
    step1 r x =
      []

reverseDFA :: DFA -> NFA
reverseDFA (qs, xs, ts, q0, as) =
  (qs, xs, ts', as, [q0])
  where
    -- existing transitions, backwards
    ts' = [((r, x), q) | ((q, x), r) <- ts]

minimiseDFA :: DFA -> DFA
minimiseDFA =
  determiniseNFA . reverseDFA . determiniseNFA . reverseDFA

-- TODO: put your definitions here
dfa1 :: DFA
dfa1 = ([1, 2, 3, 4], "ab", t, 1, [1, 2])
  where
    t =
      [ ((1, 'a'), 2),
        ((1, 'b'), 4),
        ((2, 'a'), 2),
        ((2, 'b'), 3),
        ((3, 'a'), 4),
        ((3, 'b'), 2),
        ((4, 'a'), 4),
        ((4, 'b'), 4)
      ]