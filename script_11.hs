
import Prelude hiding (even, odd)

data Expr = Const Double | Div Expr Expr
eval :: Expr -> Maybe Double
eval (Const c) = return c
eval (Div expr1 expr2) = do
  x1 <- eval expr1
  x2 <- eval expr2
  if x2 == 0 
    then Nothing
    else return (x1 / x2)


three :: Integer -> Integer
three x = 3

inf :: Integer
inf = 1 + inf

testLazy :: Integer
testLazy = three inf

even, odd :: Integer -> Bool
even n | n == 0    = True
       | otherwise = odd (n - 1)
odd n  | n == 0    = False
       | otherwise = even (n - 1)
       
sumRec :: Integer -> Integer       
sumRec 0 = 0
sumRec n = n + sumRec (n - 1) 

sumTr :: Integer -> Integer       
sumTr n = aux 0 n where
  aux acc 0 = acc
  aux acc n = aux (acc + n) (n - 1)

sumTrSeq n = aux 0 n where
  aux acc 0 = acc
  aux acc n = let accN = acc + n in seq accN (aux accN (n - 1))

sumTrBang n = aux 0 n where
  aux acc 0 = acc
  aux !acc n = aux (acc + n) (n - 1)
  
number = 10000000
testRec    = sumRec number
testTr     = sumTr number
testTrSeq  = sumTrSeq number
testTrBang = sumTrBang number


firstIndex :: (a -> Bool) -> [a] -> Int
firstIndex p = fst . head . filter (p . snd) . zip [0..]

primes :: [Integer]
primes = sieve [2..] where
  sieve (x : xs) = x : sieve (filter (\ y -> y `mod` x /= 0) xs)

first1000primes = take 1000 primes
primesBelow1000 = takeWhile (< 1000) primes
indexOfFirstPrimeAbove10000 = firstIndex (> 10000) primes