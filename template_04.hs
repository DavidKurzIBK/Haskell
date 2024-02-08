import Data.Maybe

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving (Show)

exampleTree :: Tree Float
exampleTree = Node 1 (Leaf 2) (Node 3 (Node 4 (Leaf 5) (Leaf 6)) (Leaf 7))



{- Question 1: calc height of binary tree
height of tree is defined as the longest path from root to leaf node. 
height of a leaf node is 0, height of node with 2 children is 1 plus the max. of heights of its 2 children -}
height :: Tree a -> Integer
height (Leaf x) = 0
height (Node x t1 t2) = 1 + max (height t1) (height t2) -- t1 = left-subtree, t2 = right-subtree



{- Question 2: flattens a binary tree into a list
list contains all elements of tree in order, starting from leftmost leaf 
ending at rightmost leaf-}
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node x t1 t2) = flatten t1 ++ [x] ++ flatten t2



{- Question 3: 
Define a helper function to check if a list is strictly sorted
-}
isStrictlySorted :: Ord a => [a] -> Bool
isStrictlySorted [] = True
isStrictlySorted [_] = True
isStrictlySorted (x1:x2:xs) = x1 < x2 && isStrictlySorted (x2:xs)

{- Define the main isSearchTree function which checks if binary tree is a search Tree
search tree is tree where elements in left subtree of any node are less or equal than 
to element at node, elements in right subtree of any node are greater than or equal to element
at node-}
isSearchTree :: Ord a => Tree a -> Bool
isSearchTree tree = isStrictlySorted (flatten tree)



{- Question 4: this function finds the depth of any element in a binary Tree or if an element is existing
elemDepth takes 2 args: an element and a binary tree and returns Maybe Integer
Just is the depth of element in tree, Nothing is when element doesn't exist in Tree
For a leaf node, depth of element is 1 it equals to value of the leaf node, and Nothing otherwise. 
For a node with two children, depth of the element is minimum of depths of in the left and right subtrees, otherwise, depth is Nothing.

combineDepths function takes two Maybe Integer values as arguments and returns a Maybe Integer value as the minimum of the two values 
-}
elemDepth :: Eq a => a -> Tree a -> Maybe Integer
elemDepth x (Leaf val)
  | x == val = Just 1
  | otherwise = Nothing
elemDepth x (Node val left right)
  | x == val = Just 1
  | otherwise = combineDepths (elemDepth x left) (elemDepth x right)

combineDepths :: Maybe Integer -> Maybe Integer -> Maybe Integer
combineDepths (Just d1) (Just d2) = Just (min d1 d2)
combineDepths (Just d) Nothing = Just d
combineDepths Nothing (Just d) = Just d
combineDepths Nothing Nothing = Nothing




----------------------------------------
-- Tests -------------------------------
-- Run testAll to test your functions --
----------------------------------------

testAll = tests1 >> tests2 >> tests3 >> tests4

tests1 =
  checkNumTrees "Ex 1 test 1" [3, 0, 2, 2] (height . fst)
    >> checkCharTrees "Ex 1 test 2" [3] (height . fst)

tests2 =
  checkNumTrees "Ex 2 test 1" [[2, 1, 5, 4, 6, 3, 7], [1], [-3, -2, 0, 5, 7], [-1.5, -1, -0.5, 0, 1, 1.5, 1.5]] (flatten . fst)
    >> checkCharTrees "Ex 2 test 2" [['b', 'a', 'b', 'a', 'a', 'b', 'b']] (flatten . fst)

tests3 =
  checkNumTrees "Ex 3 test 1" [False, True, True, False] (isSearchTree . fst)
    >> checkCharTrees "Ex 3 test 2" [False] (isSearchTree . fst)

tests4 =
  checkNumTrees "Ex 4 test 1" [Nothing, Nothing, Just 2, Just 0] (elemDepth 0 . fst)
    >> checkCharTrees "Ex 4 test 2" [Just 1] (elemDepth 'b' . fst)

-- internal construction of tests
exT1 = Leaf 1

exT2 = Node (-2) (Leaf (-3)) (Node 5 (Leaf 0) (Leaf 7))

exT3 = Node 0 (Node (-1) (Leaf (-1.5)) (Leaf (-0.5))) (Node 1.5 (Leaf 1) (Leaf 1.5))

exT4 = Node 'a' (Leaf 'b') (Node 'a' (Leaf 'b') (Node 'b' (Leaf 'a') (Leaf 'b')))

checkNumTrees :: (Show a, Show b) => String -> [b] -> ((Tree Float, b) -> a) -> IO ()
checkNumTrees name xs =
  checkAll name (zip [exampleTree, exT1, exT2, exT3] xs) (show . snd) (show . fst)

checkCharTrees :: (Show a, Show b) => String -> [b] -> ((Tree Char, b) -> a) -> IO ()
checkCharTrees name xs =
  checkAll name [(exT4, head xs)] (show . snd) (show . fst)

checkAll ::
  Show a =>
  String ->
  [b] ->
  (b -> String) ->
  (b -> String) ->
  (b -> a) ->
  IO ()
checkAll name xs e err c = do
  putStr ("*** " ++ name ++ ": ")
  let errors = filter (\x -> show (c x) /= e x) xs
  if null errors
    then putStrLn "OK"
    else do
      let x = head errors
      putStrLn
        ( "ERROR; expected '"
            ++ e x
            ++ "', but found '"
            ++ show (c x)
            ++ "' for value "
            ++ err x
        )