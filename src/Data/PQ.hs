-- Priority queue
module Data.PQ
  ( PQ
  , empty
  , singleton
  , push
  , pop
  , merge
  , fromList
  , toList
  ) where

data PQ a = Tree !a (PQ a) (PQ a) | Leaf deriving (Show)

empty :: (Ord a) => PQ a
empty = Leaf

singleton :: (Ord a) => a -> PQ a
singleton n = Tree n Leaf Leaf

push :: (Ord a) => PQ a -> a -> PQ a
push tree@(Tree n l r) n2 =
    if n2 < n
    then Tree n2 tree Leaf
    else Tree n r (push l n2)
push Leaf n = Tree n Leaf Leaf

pop :: (Ord a) => PQ a -> Maybe (PQ a, a)
pop (Tree n l r) = Just (merge l r, n)
pop Leaf         = Nothing


merge :: (Ord a) => PQ a -> PQ a -> PQ a
merge l@(Tree n1 l1 r1) r@(Tree n2 l2 r2) =
    if n1 < n2
    then Tree n1 r1 (merge l1 r)
    else Tree n2 r2 (merge l2 l)
merge l Leaf = l
merge Leaf r = r

fromList :: (Ord a) => [a] -> PQ a
fromList = foldl push empty

toList :: (Ord a) => PQ a -> [a]
toList pq = case pop pq of
    Just (pq2,a) -> a : toList pq2
    Nothing      -> []
