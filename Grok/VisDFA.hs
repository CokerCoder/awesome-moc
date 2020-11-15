module VisDFA where

import DFA
import Data.GraphViz
import Data.GraphViz.Attributes.Complete (Attribute (RankDir), RankDir (FromLeft))
import Data.List
import NFA

-- ----------------------------
-- The DFA visualiser front end
-- ----------------------------

-- Example usage:
-- >>> let t = [(('A','0'),'B'),(('B','0'),'C'),(('C','0'),'C')]
-- >>> visDFA ("ABC","01",t,'A',"BC")
-- Drawing DFA...
-- DFA Drawn and saved as 'dfa.png'

-- | Save a visualisation of `dfa` to file **dfa.png**.
visDFA :: (Eq a, Innovative a, Show a, Ord a, PrintDot a) => GDFA a -> IO ()
visDFA =
  visDFANamed "dfa"

-- | Save a visualisation of `dfa` to file **`name`.png**.
visDFANamed ::
  (Eq a, Innovative a, Show a, Ord a, PrintDot a) =>
  String ->
  GDFA a ->
  IO ()
visDFANamed name dfa =
  do
    putStrLn "Checking DFA..."
    putStrLn $ show $ checkDFA dfa
    putStrLn "Drawing DFA..."
    draw name (nondeterminiseDFA dfa)
    putStrLn $ "DFA drawn and saved as '" ++ name ++ ".png'"

-- | Save a visualisation of `nfa` to file **nfa.png**.
visNFA :: (Eq a, Innovative a, Show a, Ord a, PrintDot a) => GNFA a -> IO ()
visNFA nfa =
  visNFANamed "nfa" nfa

-- | Save a visualisation of `nfa` to file **`name`.png**.
visNFANamed ::
  (Eq a, Innovative a, Show a, Ord a, PrintDot a) =>
  String ->
  GNFA a ->
  IO ()
visNFANamed name nfa =
  do
    putStrLn "Checking NFA..."
    putStrLn $ show $ checkNFA nfa
    putStrLn "Drawing NFA..."
    draw name nfa
    putStrLn $ "NFA drawn and saved as '" ++ name ++ ".png'"

-- ------------------------------
-- The DFA/NFA visualiser backend
-- ------------------------------

-- | This one actually does the drawing
draw ::
  (Eq a, Innovative a, Show a, Ord a, PrintDot a) =>
  String ->
  GNFA a ->
  IO FilePath
draw name nfa =
  -- run GraphViz to layout the graph and save it as an image.
  addExtension (runGraphviz (nfaToDot nfa)) Png name

-- | Convert an NFA to a GraphViz graph specification:
nfaToDot ::
  (Eq a, Innovative a, Show a, Ord a, PrintDot a) =>
  GNFA a ->
  DotGraph a
nfaToDot (qs, xs, ts, q0s, qAs) =
  graphElemsToDot params nodes edges'
  where
    -- invisible pre-start 'states' for start arrow:
    nq0s = length q0s
    qs_ = take nq0s ((iterate (\qs -> innovate qs : qs) qs) !! nq0s)

    preStartNodes = [(q_, "") | q_ <- qs_]
    preStartEdges = [(q_, q0, "") | (q_, q0) <- zip qs_ q0s]

    -- Graph nodes:
    -- a list of nodes and their labels (including invisible pre-start
    -- 'node' with no label)
    nodes =
      preStartNodes ++ nfaNodes qs
    -- Graph edges:
    -- a list of edges and their labels (including an edge from the
    -- invisible pre-start 'node' to the start state's node)
    edges' =
      preStartEdges ++ nfaEdges ts

    -- GraphViz formatting and layout parameters:
    -- Node formatting function:
    -- show nodes as labelled circles (reject states), double circles
    -- (accept states) or no circles (invisible pre-start node)
    customNodeFmt (q, l) =
      [toLabel l, shape (stateShape q)]
      where
        stateShape q
          | q `elem` qs_ = PlainText -- invisible pre-start states
          | q `elem` qAs = DoubleCircle -- accept states
          | otherwise = Circle -- non-accept states
          -- Edge formatting function:
          -- show edges as labelled transition arrows
    customEdgeFmt (_, _, l) =
      [toLabel l, arrowTo vee]
    -- Other formatting and layout attributes:
    globalAttrs =
      [GraphAttrs [RankDir FromLeft], NodeAttrs [], EdgeAttrs []]
    -- params :: GraphvizParams Int String String () String
    params =
      nonClusteredParams
        { globalAttributes = globalAttrs,
          isDirected = True,
          fmtNode = customNodeFmt,
          fmtEdge = customEdgeFmt
        }

-- dfaNodes
-- Convert a list of states into a list of
-- (state, state-name) pairs
nfaNodes :: (Show a) => [a] -> [(a, String)]
nfaNodes qs =
  [(q, show q) | q <- qs]

-- dfaEdges
-- Convert a transition function into a list of
-- (from-state, to-state, edge-label) triples
nfaEdges :: (Eq a) => [GTransn a] -> [(a, a, String)]
nfaEdges ts =
  [(q, r, intersperse ',' (sort xs)) | ((q, r), xs) <- zip edges' labels]
  where
    edges' = nub (edges ts)
    labels = map (\(q, r) -> [x | ((q', x), r') <- ts, q' == q, r' == r]) edges'