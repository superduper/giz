{-# LANGUAGE RankNTypes #-}
module Kruskal (
 main
) where

import           Control.Monad          (filterM)
import           Data.Equivalence.Monad
import           Data.Graph             as G
import           Data.List              (sortBy)
import           Data.Map               as Map
import           Data.Maybe             (fromJust)
import           Data.Ord               (comparing)
import           Debug.Trace

-- Current implementation uses EquivalenceMonad implementation
-- of Tarjan's union-find algo done by https://bitbucket.org/paba/equivalence

kruskal :: Ord a =>
           (Edge -> a) -> Graph -> [(Vertex, Vertex)]
kruskal weight graph = runEquivM' $ filterM go sorted
  where
      -- first we sort edge set by weight
      edges'   = G.edges graph
      sorted   = (sortBy cmp edges')
      cmp      = comparing weight
      -- actual union&find happens here
      -- this is a monadic filter that runs inside EquivST
      -- and filters out other nodes
      go (u,v) = do eq <- equivalent u v           -- we check whether we have an intersection in our sets
                    traceM $ "equating " ++ show (u, v, eq)
                    if eq then return False        -- we have it in our vert set
                    else equate u v >> return True -- we don't have it

fromL :: forall c a. Ord a => [(a, c)] -> a -> c
fromL xs = fromJust . flip Map.lookup (Map.fromList xs)

main :: IO () 
main = mapM_ (putStrLn . show) $ kruskal w g
  where
    -- edges + weights
    w = fromL [ ((1,2),1)
              , ((2,3),4)
              , ((3,4),5)
              , ((1,4),30)
              , ((1,3),4) ]
    -- Builds an adjacency list representation of a graph, 
    -- mapping each vertex to its list of successors.
    g   = G.buildG  (1,4) [ (1,2)
                          , (2,3)
                          , (3,4)
                          , (1,4)
                          , (1,3)]
