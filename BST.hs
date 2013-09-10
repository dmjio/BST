module BST
    (-- * Top level Type and Data constructors
     BST(..)
      -- * Tree insertion
    , insert
      -- * Tree traversals
    , iot
    , pre
    , post
     -- * Min/Max Functions
    , min
    , max
     -- * Root
    , root
    ) where

import           Prelude hiding (max, min)

data BST a = Empty -- ^ Empty Leaf
           | Node a (BST a) (BST a) -- ^ Node
           deriving (Show)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node y l Empty) | x > y     = Node y l (Node x Empty Empty)
                          | x < y     = Node y (insert x l) Empty
                          | otherwise = error "duplicate node"
insert x (Node y Empty r) | x < y     = Node y (Node x Empty Empty) r
                          | x > y     = Node y Empty (insert x r)
                          | otherwise = error "duplicate node"
insert x (Node y l r)     | x < y     = Node y (insert x l) r
                          | x > y     = Node y l (insert x r)
                          | otherwise = error "duplicate node"

-- | Minimum Node
min :: BST a -> a
min Empty = error "Empty Tree"
min (Node x Empty Empty) = x
min (Node x Empty r) = x
min (Node x l _) = min l

-- | Maximum Node
max :: BST a -> a
max Empty = error "Empty Tree"
max (Node x Empty Empty) = x
max (Node x l Empty) = x
max (Node x _ r) = max r

-- | Root
root :: BST a -> a
root (Node x _ _) = x
root _ = error "empty tree"

-- | Pre-Order Traversal, root, traverse left, traverse right
pre :: BST a -> [a]
pre Empty = []
pre (Node x l r) = x : pre l ++ pre r

-- | In-Order Traversal, traverse left, root, traverse right
iot :: BST a -> [a]
iot Empty = []
iot (Node x l r) = iot l ++ [x] ++ iot r

-- | Post-Order Traversal, traverse left, traverse right, root
post :: BST a -> [a]
post Empty = []
post (Node x l r) = post l ++ post r ++ [x]

instance Functor BST where
    fmap _ Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)











