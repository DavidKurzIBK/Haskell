

data Expr = Number Integer | Plus Expr Expr | Negate Expr
data List a = Empty | Cons a (List a)

len :: List a -> Int
len Empty = 0
len (Cons _ xs) = 1 + len xs

first :: List a -> a
first (Cons x _) = x

append (Cons x xs) ys = Cons x (append xs ys)
append Empty ys = ys


lastElem (Cons _ xs@(Cons _ _)) = lastElem xs
lastElem (Cons x _)             = x     -- here the order of eq. matters
lastElem Empty                  = error "empty list has no last element"

numbers (Number x)   = Cons x Empty
numbers (Plus e1 e2) = append (numbers e1) (numbers e2)
numbers (Negate e)   = numbers e


remdups Empty = Empty
remdups (Cons x xs) = Cons x (remove x (remdups xs))
-- subtask: define "remove x" to delete each x from list xs
remove x Empty = Empty
remove x (Cons y ys) = rHelper (x == y) y (remove x ys)
rHelper True  _ xs = xs
rHelper False y xs = Cons y xs

data IntList = EmptyIL | ConsIL Integer IntList

lenIL EmptyIL       = 0
lenIL (ConsIL _ xs) = 1 + lenIL xs

data StringList = EmptySL | ConsSL String StringList

lenSL EmptySL       = 0
lenSL (ConsSL _ xs) = 1 + lenSL xs

f :: a -> b -> a
f x y = x       

g :: Num a => a -> a -> a
g x y = x + y - 3        

h :: Ord a => a -> a -> String
h x y = "cmp is " ++ show (x < y) 

i :: (Num a, Show a) => a -> String
i x = "result: " ++ show (x + 3)  

-- renamed rHelper to repHelper, since name rHelper was used above
replace :: Eq a => a -> a -> List a -> List a
replace _ _ Empty = Empty
replace x y (Cons z zs) = repHelper (x == z) y z (replace x y zs)
repHelper True  y _ xs = Cons y xs
repHelper False _ z xs = Cons z xs

replaceAB :: List Char -> List Char
replaceAB xs = replace 'A' 'B' xs

-- renamed version of append using predefined lists
appendPredefined :: [a] -> [a] -> [a]
appendPredefined [] ys       = ys
appendPredefined (x : xs) ys = x : appendPredefined xs ys

data Unit = Unit                   -- tuple with 0 entries
data Pair a b = Pair a b           -- tuple with 2 entries
data Triple a b c = Triple a b c   -- tuple with 3 entries

findY :: [Pair Char a] -> a
findY []               = error "..."
findY (Pair 'y' v : _) = v
findY (_ : xs)         = findY xs

-- renamed version of findY using predefined pairs
findYPredefined :: [(Char, a)] -> a
findYPredefined []             = error "..."
findYPredefined (('y', v) : _) = v
findYPredefined (_ : xs)       = findYPredefined xs

divSafe :: Double -> Double -> Maybe Double
divSafe x 0 = Nothing
divSafe x y = Just (x / y)

-- renamed version of Expr
data Expr2 = Plus2 Expr2 Expr2 | Div2 Expr2 Expr2 | Number2 Double

eval :: Expr2 -> Maybe Double
eval (Number2 x) = Just x
eval (Plus2 x y) = plusMaybe (eval x) (eval y)
eval (Div2 x y)  = divMaybe (eval x) (eval y)

plusMaybe (Just x) (Just y) = Just (x + y)
plusMaybe _ _               = Nothing

divMaybe (Just x) (Just y) = divSafe x y
divMaybe _ _               = Nothing

-- renamed version of divSafe example using Either type
divSafe2 :: Double -> Double -> Either String Double
divSafe2 x 0 = Left ("don't divide " ++ show x ++ " by 0")
divSafe2 x y = Right (x / y)

eval2 :: Expr2 -> Either String Double
eval2 (Number2 x) = Right x
eval2 (Plus2 x y) = plusEither (eval2 x) (eval2 y)
eval2 (Div2 x y)  = divEither (eval2 x) (eval2 y)

divEither (Right x) (Right y) = divSafe2 x y
divEither e@(Left _) _        = e     -- new case analysis required
divEither _ e                 = e

plusEither (Right x) (Right y) = Right (x + y)
plusEither e@(Left _) _        = e     -- new case analysis required
plusEither _ e                 = e