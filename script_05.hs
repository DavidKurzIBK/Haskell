type Month = Int
type Day = Int
type Year = Int
type Date = (Day, Month, Year)

createDate :: Day -> Month -> Year -> Date
createDate d m y = (d, m, y)

data Expr a = Var String | Number a | Plus (Expr a) (Expr a)
eval :: Num a => [(String,a)] -> Expr a -> a
eval ass (Number x) = x 
eval ass (Plus e1 e2) = eval ass e1 + eval ass e2
eval ass (Var x) = case lookup x ass of
  Just i -> i
  _ -> error ("assignment does not include variable " ++ x)

and1 b1 b2 = case b1 of
  True -> case b2 of
    True -> True
    False -> False

and2 b1 b2 = case b1 of
  True -> case b2 of
    True -> True
  False -> False

numRootsLet a b c = let 
    disc = b^2 - 4 * a * c   -- local variable
    analyse EQ = 1           -- local function
    analyse LT = 0
    analyse GT = 2
  in analyse (compare disc 0)

numRootsWhere a b c = analyse (compare disc 0) where
    disc = b^2 - 4 * a * c   -- local variable
    analyse EQ = 1           -- local function
    analyse LT = 0
    analyse GT = 2

numRootsGuarded a b c
  | disc > 0   = 2
  | disc == 0  = 1
  | otherwise  = 0             -- otherwise = True
  where disc = b^2 - 4 * a * c -- disc is shared among cases

roots :: Double -> Double -> Double -> (Double, Double)
roots a b c 
  | a == 0 = error "not quadratic"
  | d <  0 = error "no real roots"
  | otherwise = ((- b - r) / e, (- b + r) / e)
  where d = b * b - 4 * a * c
        e = 2 * a
        r = sqrt d

sumRoots :: Double -> Double -> Double -> Double
sumRoots a b c = let
    (x, y) = roots a b c  -- pattern match in let
  in x + y      

rootsMaybe :: Double -> Double -> Double -> Maybe (Double, Double)
rootsMaybe a b c 
  | a == 0 = Nothing
  | d <  0 = Nothing
  | otherwise = Just ((- b - r) / e, (- b + r) / e)
  where d = b * b - 4 * a * c
        e = 2 * a
        r = sqrt d

sumRootsMaybe :: Double -> Double -> Double -> Maybe Double
sumRootsMaybe a b c = 
  case rootsMaybe a b c of      -- case for explicit error handling
    Just (x, y) -> Just (x + y) -- nested pattern matching
    n -> Nothing                -- can't be replaced by "n -> n"! (types)

factorialD :: Integer -> Integer
factorialD 0 = 1
factorialD n = n * factorialD (n - 1)

factorialU :: Integer -> Integer
factorialU n = fact 1 1 where
  fact r i
    | i <= n = fact (i * r) (i + 1) 
    | otherwise = r

factorial :: Integer -> Integer
factorial n = product [1 .. n]