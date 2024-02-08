module Tree (
	Tree(..)
	splitAtLevel,
	fillXs
) where 

data Tree a = X | Node (Tree a) a (Tree a)
deriving (Show, Eq)



splitAtLevel :: Int -> Tree a -> (Tree a, [Tree a])
splitAtLevel 0 t = (X, [t])
splitAtLevel _ X = (X, [X])
splitAtLevel n (Node x left right) =
  let
    (takenLeft, remainingLeft) = splitAtLevel (n-1) left
    (takenRight, remainingRight) = splitAtLevel (n-1) right
  in
    (Node x takenLeft takenRight, remainingLeft ++ remainingRight)



fillXs :: Tree a -> [Tree a] -> (Tree a, [Tree a])
fillXs X (t:ts) = (t, ts)  -- Replace X with a tree from the list
fillXs (Node x left right) ts =
  let
    (filledLeft, remainingTrees1) = fillXs left ts
    (filledRight, remainingTrees2) = fillXs right remainingTrees1
  in
    (Node x filledLeft filledRight, remainingTrees2)
fillXs t [] = (t, [])  -- If the list is empty, return the original tree

