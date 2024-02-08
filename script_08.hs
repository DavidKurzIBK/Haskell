sum' :: Num a => [a] -> a
sum' = foldr (+) 0

product' :: Num a => [a] -> a
product' = foldr (*) 1

concat' :: [[a]] -> [a]
concat' = foldr (++) []

(+++) :: [a] -> [a] -> [a]
xs +++ ys = foldr (:) ys xs
 
length' :: [a] -> Int
length' = foldr (\ _ -> (+ 1)) 0 

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr ((&&) . f) True

maximum' :: Ord a => [a] -> a
maximum' = foldr1 max

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

isSorted :: Ord a => [a] -> Bool
isSorted xs = all id $ zipWith (<=) xs (tail xs)

evenSquares100 = sum [ x^2 | x <- [0 .. 100], even x]

prime n = n >= 2 && null [ x | x <- [2 .. n - 1], n `mod` x == 0] 

pairs n = [ (i, j) | i <- [0..n], even i, j <- [0..i]]

foo zs = [ x + y + z | 
   x <- [0..20], 
   even x, 
   let y = x * x, 
   y < 200, 
   Just z <- zs]

ptriple x y z = x^2 + y^2 == z^2
ptriples n = [ (x,y,z) | 
  x <- [1..n], y <- [x..n], z <- [y..n], ptriple x y z]
  