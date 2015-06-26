module Trees (
main
) where

import           Data.List  (find)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

data DFSKind = PreOrder | InOrder | PostOrder

-- | Deep First Search. Returns a list of nodes of a given tree.

dfs :: Tree a      -- ^ Tree to traverse
    -> (a -> Bool) -- ^ Predicate
    -> DFSKind     -- ^ Traversal kind
    -> Maybe a     -- ^ Search result

dfs EmptyTree p _ = Nothing

dfs (Node v EmptyTree EmptyTree) p _
  | p v       = Just v
  | otherwise = Nothing

dfs t p PreOrder = find p $ traverse t
   where
     traverse EmptyTree                = []
     traverse (Node val left right)  = [val] ++ (traverse left) ++ (traverse right)

dfs t p PostOrder = find p $ traverse t
   where
     traverse EmptyTree               = []
     traverse (Node val left right) = (traverse left) ++ (traverse right) ++ [val]

dfs t p InOrder = find p $ traverse t
   where
     traverse EmptyTree                = []
     traverse (Node val left right)  = (traverse left) ++ [val] ++ (traverse right)


-- | Breath First Search. Returns a list of nodes of a given tree.
bfs :: Tree a     -- ^ Tree to traverse
    -> (a -> Bool) -- ^ Predicate
    -> Maybe a    -- ^ Search result
bfs t p = find p $ traverse [t]
    where traverse []                         = []
          traverse (EmptyTree           : xs) = traverse xs
          traverse (Node val left right : xs) = val : traverse (xs ++ [left, right])

-- | Sample tree
tree :: Tree Int
tree = Node 1
            (Node 2
                  (Node 4
                        (Node 7 EmptyTree EmptyTree)
                        EmptyTree)
                  (Node 5 EmptyTree EmptyTree))
            (Node 3
                  (Node 6
                        (Node 8 EmptyTree EmptyTree)
                        (Node 9 EmptyTree EmptyTree))
                  EmptyTree)

main :: IO ()
main = do print $ dfs tree query InOrder
          print $ dfs tree query PostOrder
          print $ dfs tree query PreOrder
          print $ bfs tree query
  where
    query y = y == 8
