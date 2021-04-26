module SkewHeap where

import Text.Printf



data SkewHeap a = Empty | Node a (SkewHeap a) (SkewHeap a)

instance Show a => Show (SkewHeap a) where
  show = showAtLevel 0
    where
      showAtLevel l Empty = addSpace l ++ show "Empty"
      showAtLevel l (Node x h1 h2) =
            printf "%s%s\n%s\n%s" (addSpace l)
                (show x) (showAtLevel (l + 1) h1) (showAtLevel (l + 1) h2)
      addSpace = flip replicate '\t'


-- O(log n)
merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge h1 Empty = h1
merge Empty h2 = h2
merge t1@(Node ax ah1 ah2) t2@(Node bx bh1 bh2)
    | ax <= bx  = Node ax (merge ah2 t2) ah1
    | otherwise = Node bx (merge bh2 t1) bh1


-- O(log n)
insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert x = merge (Node x Empty Empty)


-- O(n)
delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete z Empty = Empty
delete z (Node x h1 h2) 
    | z == x    = merge h1 h2
    | otherwise = Node x (delete z h1) (delete z h2) 


-- O(log n)
extractRoot :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
extractRoot Empty = Nothing
extractRoot (Node x h1 h2) = Just (x, merge h1 h2)


-- O(n log n)
sorted :: Ord a => SkewHeap a -> [a]
sorted Empty          = []
sorted (Node x h1 h2) = x : sorted (merge h1 h2)


-- O(n) + O(log n) = O(n)
replaceElement :: Ord a => a -> a -> SkewHeap a -> SkewHeap a
replaceElement old new h = insert new $ delete old h

