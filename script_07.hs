import Data.Char

nTimes :: (a -> a) -> Int -> a -> a
nTimes f n x 
  | n == 0 = x
  | otherwise = f (nTimes f (n - 1) x)

tower :: Integer -> Int -> Integer -- tower x n = x ^ (x ^ ... (x ^ 1)) 
tower x n = nTimes (x ^) n 1       -- n exponentiations with basis x

replicate' :: Int -> a -> [a]      -- replicate n x = [x, ..., x]
replicate' n x = nTimes (x :) n [] -- n insertions of x

selectFunction :: Bool -> (Int -> Int) -- same as Bool -> Int -> Int
selectFunction True  = (* 3)
selectFunction False = abs

example = nTimes (\ x -> 3 * (x + 1))

multTwo = map (2 *)
toUpperList = map toUpper


elem' :: Eq a => a -> [a] -> Bool
elem' x xs = filter (== x) xs /= []

-- the well known lookup function
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' x xs = case filter (\ (k,_) -> x == k) xs of
  [] -> Nothing
  ((_,v) : _) -> Just v

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) =   -- x is pivot element
  qsort (filter (<= x) xs) ++ [x] ++ qsort (filter (> x) xs)

exampleSlide17 = length . show . (* 7) . abs . (+ 5)
