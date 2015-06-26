module Path (
  main
) where

import           Data.List
import qualified Data.List.Key as K
import           Data.Map      (Map, adjust, fromList, toList, fromListWith, keys, (!))


type Weight = Float

dijkstra :: (Show v, Ord v) => v     -- ^ Start vertix
         -> Map v [(v, Weight)]      -- ^ Graph
         -> Map v (Weight, Maybe v)  -- ^ Shortest path vertices
dijkstra source graph =
    -- initial step: mark every node weight with infinity distances and fire traversal
    f (fromList [(v, (if v == source then 0 else inf, Nothing))
                | v <- keys graph]) (keys graph) where
    -- Edge case to end node traversal, when no nodes left to visit
    -- We return a Map [Vertix -> (Weight, Vertix)]
    -- where map value is a neighb vertix with lowest weight
    f ds [] = ds
    -- Node traversal, ds here is a part of source graph left to traverse
    -- Recursion here used instead of cycle, so it works till we have all
    -- reachable keys marked.
    f ds q  = f (foldr relax ds minNodeLabel) (delete minNode q)
              -- find next node with minimum weight
              where minNode       = K.minimum (fst . (ds !)) q
                    minNodeLabel  = graph ! minNode
                    minNodeWeight = fst (ds ! minNode)
                    -- adjusts minimum distance to start vertix
                    -- if we have a lesser calculation then replace
                    relax (e,d)  = let maybeBetterOne = min (relaxedWeight,  Just minNode)
                                       relaxedWeight  = minNodeWeight + d
                                   in adjust maybeBetterOne e
    -- Our celever alias to repersent infinity, leveraging haskell laziness
    inf     = 1/0

shortestPath :: (Show a, Ord a) => a -- ^ Starting vertix
             -> a                    -- ^ Endpoint vertix
             -> Map a [(a, Weight)]  -- ^ Graph
             -> [a]                  -- ^ Resulting path
shortestPath from to graph = reverse $ f to where
    f x = x : maybe [] f (snd $ dijkstra from graph ! x)


-- Convert a list of edges weights to a vertex -> edges map
mkGraph :: (Ord a, Show a) => [(a, a, Weight)] -> Map a [(a, Weight)]
mkGraph g = fromListWith (++) $ g >>=
               \(a,b,d) -> [(a,[(b,d)]), (b,[(a,d)])]
-- print $ shortestPath 'y' 'z' g == "yfz"
main :: IO ()
main = mapM_ printW $ toList d
   --    print $ shortestPath 'a' 'i' b == "abi"
    where  d = dijkstra 'a' b
           g = mkGraph [ ('y','f', 3)
                      , ('b','y', 4)
                      , ('y','d', 6)
                      , ('b','d', 8)
                      , ('f','d', 7)
                      , ('f','z', 5)
                      , ('d','z', 17)]
           printW (a, (b, _)) = putStrLn $ show (a, b)

           b = mkGraph [ ('a', 'b', 3)
                      , ('b', 'c', 2)
                      , ('c', 'd', 1)
                      , ('d', 'e', 2)
                      , ('e', 'f', 1)
                      , ('h', 'f', 3)
                      , ('d', 'h', 1)
                      , ('f', 'g', 2)
                      , ('h', 'g', 1)
                      , ('h', 'i', 4)
                      , ('g', 'i', 2)
                      , ('j', 'g', 3)
                      , ('i', 'j', 3)
                      , ('a', 'j', 1)
                      , ('b', 'i', 1)
                      , ('c', 'i', 4)]