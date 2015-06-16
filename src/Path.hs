module Path (
  buildGraph
, shortestPath
, demo
) where

import           Data.List
import qualified Data.List.Key as K
import           Data.Map      (Map, adjust, fromList, fromListWith, keys, (!))

dijkstra :: Ord a => a -> Map a [(a, Float)] -> Map a (Float, Maybe a)
dijkstra source graph =
    -- initial step: mark every node weight with infinity distances and fire traversal
    f (fromList [(v, (if v == source then 0 else inf, Nothing))
                | v <- keys graph]) (keys graph) where
    -- Our celever alias to repersent infinity, leveraging haskell laziness
    inf     = 1/0
    -- Edge case to end node traversal, when no nodes left to visit
    f ds [] = ds
    -- Node traversal, ds here is a part of source graph left to traverse
    f ds q  = f (foldr relax ds $ graph ! m) (delete m q) where
              -- takes closest node opposing to a current visited node
              m = K.minimum (fst . (ds !)) q
              -- adjusts minimum distance, for every node connected to m
              relax (e,d) = adjust (min (fst (ds ! m) + d, Just m)) e

shortestPath :: Ord a => a -> a -> Map a [(a, Float)] -> [a]
shortestPath from to graph = reverse $ f to where
    f x = x : maybe [] f (snd $ dijkstra from graph ! x)

-- Convert a list of edges weights to a vertex -> edges maap
buildGraph :: Ord a => [(a, a, Float)] -> Map a [(a, Float)]
buildGraph g = fromListWith (++) $ g >>=
               \(a,b,d) -> [(a,[(b,d)]), (b,[(a,d)])]

demo :: IO ()
demo = do let g = buildGraph [('a','c',2), ('a','d',6), ('b','a',3)
                             ,('b','d',8), ('c','d',7), ('c','e',5)
                             ,('d','e',10)]
          print $ shortestPath 'a' 'e' g == "ace"
