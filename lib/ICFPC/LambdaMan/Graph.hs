module ICFPC.LambdaMan.Graph where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Monoid
import ICFPC.Dijkstra
import ICFPC.LambdaMan

import Debug.Trace


asGraph :: Input -> Map (Int, Int) (Map (Int, Int) Direction)
asGraph input = M.fromListWith M.union $ execWriter genEdges
  where
    (width, height) = input.size

    genEdges = for_ [0 .. height - 1] \y ->
      for_ [0 .. width - 1] \x -> do
        let p = (x, y)
        when (validDestination input p) do
          let p' = (x - 1, y)
          when (validDestination input p') do
            tell [(p, M.singleton p' L), (p', M.singleton p R)]
          let p'' = (x, y - 1)
          when (validDestination input p'') do
            tell [(p, M.singleton p'' U), (p'', M.singleton p D)]

reversePath :: [Direction] -> [Direction]
reversePath = reverse . map \case
  U -> D
  D -> U
  L -> R
  R -> L

compactGraph
  :: Ord k
  => (k -> Bool) -> Map k (Map k Direction) -> Map k (Map k [Direction])
compactGraph exempt m0 = go edges (M.map (M.map pure) m0)
  where
    edges = [(i, j) | (i, js) <- M.toList m0, j <- M.keys js]
    go ((!i, !j):es) !m
      | not $ exempt j
      , Just is <- M.lookup i m
      , M.size is <= 2
      , Just js <- M.lookup j m
      , M.size js == 2
      , Just itoj <- M.lookup j is
      , [(r, jtor)] <- M.toList $ M.delete i js
        -- ? - i - j - r {
        --         ^ not exempt
      , itor <- itoj ++ jtor
      = go ((i, r):(r, i):es)
        $ M.insertWith M.union r (M.singleton i $ reversePath itor)
        $ M.insertWith M.union i (M.singleton r itor)
        $ M.mapMaybe (mfilter (not . M.null) . Just . M.delete j)
        $ M.delete j m
      | otherwise = go es m
    go [] m = m

renameGraph :: Ord k => Map k (Map k v) -> (Map Int (Map Int v), Map k Int)
renameGraph m = (m', naming)
  where
  (!m', (!naming, _)) = runState
    (M.fromList <$> traverse goOuter (M.toList m))
    (M.empty, 0)
  goOuter (!i, inner) = (,) <$> rename i
    <*> (M.fromList <$> traverse goInner (M.toList inner))
  goInner (!i, v) = (,v) <$> rename i

  rename i = state \(!n, !fresh)
    -> case M.insertLookupWithKey (\_ _new old -> old) i fresh n of
      (Nothing, n') -> (fresh,) $! (n',) $! fresh + 1
      (Just v, n') -> (v, (n', fresh))

breadthFirstGraph :: Int -> Map Int (Map Int [Direction]) -> [Direction]
breadthFirstGraph initV distances
  = trace ("Renamed graph: " <> graph2Dot distances)
  $ case dijkstra initial (IS.null . snd) adjacent of
    Just (_, segments) -> segments >>= \(i, j) -> distances M.! i M.! j
    Nothing -> error "No solution"
  where
    idistances = IM.fromAscList
      . map (second $ IM.fromAscList . map (second length) . M.toList)
      . M.toList $ distances
    initial =
      ( initV
      , IS.delete initV $ IS.fromAscList $ M.keys distances
      )
    adjacent (!i, !pills) =
      [ ((j, IS.delete j pills), Sum w, (i, j))
      | (j, w) <- IM.toList $ idistances IM.! i
      ]

breadthFirstGraphSolution :: Input -> [Direction]
breadthFirstGraphSolution input = breadthFirstGraph initV renamed
  where
    initV = naming M.! input.start
    (renamed, naming) = renameGraph compacted
    compacted = compactGraph (== input.start) graph
    graph = asGraph input

graph2Dot :: Map Int (Map Int v) -> String
graph2Dot m = "graph { " <> intercalate "; " edges <> " }"
  where
    edges =
      [ show i <> " -- " <> show j
      | (i, js) <- M.toList m
      , (j, _) <- M.toList js
      , i < j
      ]
