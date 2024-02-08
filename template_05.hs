{- Exercise 1 -}


{-keywords: type & data used to define new types!!: 
type: for aliases for existing types, like string, Integer, boolean..
data: -"- definning new types from scratch-}

type Age = (String, Integer)
data Person = Person String Integer

exampleAges :: [Age]
exampleAges = [("Alice", 17), ("Bob", 35), ("Clara", 17)]


-- Question 1.2
{-ticketCostA: nested if-then-else expressions to check age and determine the ticket cost. 
formatCost function is defined using a let-expression.
ticketCostB: guarded equations to check age and determine the ticket cost. 
formatCost function is defined using a where-construct.
"otherwise"-keyword is equivalent to True and often used in the last guard of a sequence of guards
Both implementations are equivalent, guarded-equations version is generally considered to be more idiomatic Haskell.
-}

ticketCostA :: Age -> String
ticketCostA (name, age) =
  let
    formatCost cost = name ++ " pays " ++ cost ++ " euros for a ticket"
  in
    if age >= 18
      then formatCost "15"
      else if age >= 13
        then formatCost "7.50"
        else formatCost "5"

ticketCostB :: Age -> String
ticketCostB (name, age)
  | age >= 18 = formatCost "15"
  | age >= 13 = formatCost "7.50"
  | otherwise = formatCost "5"
  where
    formatCost cost = name ++ " pays " ++ cost ++ " euros for a ticket"
	


-- Question 1.3
{-base case: the list of ages is empty ([]). 
Then function returns Nothing, cause there are no people with the specified age.
recursive case: checks whether the age of the current person (at head of list) matches the targetAge. 
If it does, recursively calls ageLookup on rest of the list (from tail of list) and appends the current name to the result. 
If it doesn't match, it continues the recursive search on the rest of the list.
Implementation uses pattern matching + a case ... of ... expression to handle Just and Nothing cases.
rest variable refers to tail of list of ages that is passed to the function. 
It is used to recursively search through the list of ages, looking for the specified age.
 -}

ageLookup :: [Age] -> Integer -> Maybe [String]
ageLookup [] _ = Nothing
ageLookup ((name, age):rest) targetAge =
  case age == targetAge of
    True  -> case ageLookup rest targetAge of
               Just names -> Just (name : names)
               Nothing    -> Just [name]
    False -> ageLookup rest targetAge



-- Question 1.4
{- keys of shape Left l, function performs lookup on left half of the pairs and returns other half of first matching pair, wrapped in Just. 
If no match found, returns Nothing.
keys of shape Right r, function first swaps pairs using the swap function (exchanges elements each pair),performs lookup on right half of swapped pairs. 
If a match found, returns left half of pair wrapped in Just. 
If no match found, it returns Nothing.
swap function: facilitate the lookup for Right r keys, as it converts the original pairs into pairs with the elements swapped.
-}

bidirectionalLookup :: (Eq b, Eq a) => Either a b -> [(a, b)] -> Maybe (Either a b)
bidirectionalLookup (Left l) pairs = case lookup l pairs of
  Just r  -> Just (Right r)
  Nothing -> Nothing
bidirectionalLookup (Right r) pairs = case lookup r (map swap pairs) of
  Just l  -> Just (Left l)
  Nothing -> Nothing
  where
    swap (x, y) = (y, x)




{- Exercise 2 -}

data Tree a = Node a (Tree a) (Tree a) | X deriving Show

exampleTree = Node 1 (Node 2 X X) (Node 3 X (Node 4 X X))

{-base cases: n is 0 or tree is X, then X is returned.
recursive case (tree is a Node), function constructs new Node with same value x 
and recursively calls takeLevels on left and right subtrees with n-1 as argument.
Implementation should correctly produce a tree consisting of the upper n levels of the input tree 
.. so traverse the tree and stop when the level exceeds the specified limit (n)..
-}
takeLevels :: Int -> Tree a -> Tree a
takeLevels 0 _ = X
takeLevels _ X = X
takeLevels n (Node x left right) = Node x (takeLevels (n-1) left) (takeLevels (n-1) right)


{- use recursion to traverse tree and remove Nodes of upper n levels and correctly produce list of trees representing the remaining subtrees
dropLevels function return a list of trees, each tree corresponds to remaining subtrees after removing Nodes of upper n levels
base cases: n is 0 or tree is X....cases, function returns list containing only input tree or X.
recursive case (when tree is a Node), function recursively calls dropLevels on left and right subtrees with n-1 as the argument. then concatenates the resulting lists.
-}
dropLevels :: Int -> Tree a -> [Tree a]
dropLevels _ X = [X]
dropLevels 0 t = [t]
dropLevels n (Node _ left right) = dropLevels (n-1) left ++ dropLevels (n-1) right


{-implement splitAtLevel function, use recursion to traverse tree and 
create a tuple containing tree representing first n levels and list of trees representing remaining subtrees
base cases: when n is 0 or the tree is X. Then function returns tuple with X representing taken part and  
list containing only input tree or X representing the remaining part.
recursive case (when tree is a Node), function recursively calls splitAtLevel on left and right subtrees with n-1 as the argument. 
then constructs new Node using results and concatenates lists of remaining subtrees.
-}
splitAtLevel :: Int -> Tree a -> (Tree a, [Tree a])
splitAtLevel 0 t = (X, [t])
splitAtLevel _ X = (X, [X])
splitAtLevel n (Node x left right) =
  let
    (takenLeft, remainingLeft) = splitAtLevel (n-1) left
    (takenRight, remainingRight) = splitAtLevel (n-1) right
  in
    (Node x takenLeft takenRight, remainingLeft ++ remainingRight)


{- base case: current node is X. In this case, the function takes a tree from the list to fill the X and returns the filled tree along with the remaining trees.
recursive case (when tree is Node), function recursively calls fillXs on left and right subtrees, updating list of remaining trees accordingly.
implementation should correctly fill in Xs in input tree using provided list of trees, returning filled tree and remaining trees that did not replace any Xs
-}
fillXs :: Tree a -> [Tree a] -> (Tree a, [Tree a])
fillXs X (t:ts) = (t, ts)  -- Replace X with a tree from the list
fillXs (Node x left right) ts =
  let
    (filledLeft, remainingTrees1) = fillXs left ts
    (filledRight, remainingTrees2) = fillXs right remainingTrees1
  in
    (Node x filledLeft filledRight, remainingTrees2)
fillXs t [] = (t, [])  -- If the list is empty, return the original tree






{- Tests -}
checkEx1 = sequence_ [test1, test2, test3, test4]

checkEx2 = mapM_ putStrLn  [test5, test6, test7, test8, test9, test10]

-- Internal construction of tests
test1 = mapM_ (putStrLn . (\((n, a), c) -> check "ticketCostA" (show $ n ++ " pays " ++ c ++ " euros for a ticket") (ticketCostA (n, a)))) (zip testAges testCosts)

test2 = mapM_ (putStrLn . (\((n, a), c) -> check "ticketCostB" (show $ n ++ " pays " ++ c ++ " euros for a ticket") (ticketCostB (n, a)))) (zip testAges testCosts)

test3 =
  mapM_
    (putStrLn . (\(i, o) -> check "ageLookup" (show o) (ageLookup testAges i)))
    (zip [50, 13, 12] [Just ["D"], Nothing, Just ["B", "E"]])

test4 =
  mapM_
    (putStrLn . (\(i, o) -> check "bidirectionalLookup" (show o) (bidirectionalLookup i testAges)))
    (zip [Right 0, Right 12, Left "E", Left "F"] [Just (Left "A"), Just (Left "B"), Just (Right 12), Nothing])

test5 = check "test1" (show $ Node 1 (Node 2 X X) (Node 3 X X)) (takeLevels 2 exampleTree)
test6 = check "test2" (show [X, X, X, Node 4 X X]) (dropLevels 2 exampleTree)
test7 = check "test3" (show (Node 1 (Node 2 X X) (Node 3 X X), [X, X, X, Node 4 X X])) (splitAtLevel 2 exampleTree)
test8 = check "test4" (show (exampleTree, [] :: [Tree Int])) (uncurry fillXs $ splitAtLevel 0 exampleTree)
test9 = check "test5" (show (exampleTree, [] :: [Tree Int])) (uncurry fillXs $ splitAtLevel 2 exampleTree)
test10 = check "test6" (show (exampleTree, [] :: [Tree Int])) (uncurry fillXs $ splitAtLevel 5 exampleTree)

testAges = [("A", 0), ("B", 12), ("C", 17), ("D", 50), ("E", 12)]
testCosts = ["5", "5", "7.50", "15", "5"]

check name e c =
  "*** " ++ name ++ ": " ++ (
    if show c == e then "OK"
    else "ERROR; expected '" ++ e ++ "', but found '" ++ show c ++ "'")