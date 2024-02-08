class Equality a where
  equal :: a -> a -> Bool      -- equality
  different :: a -> a -> Bool  -- inequality
  -- properties:
  --   equal x x should evaluate to True
  --   equal and different should be symmetric
  --   exactly one of equal x y and different x y should be True
  equal x y = not (different x y)   -- default implementation
  different x y = not (equal x y)   -- default implementation

fractionalPart :: RealFrac a => a -> a 
fractionalPart x = x - fromInteger (floor x)

data Complex a = Complex a a -- polymorphic: type a instead of Double

instance Eq a => Eq (Complex a) where
  Complex r1 i1 == Complex r2 i2 = r1 == r2 && i1 == i2
  -- comparing r1 and r2 (i1 and i2) requires equality on type a

-- for Show not only Show a is required, but also Ord a and Num a 
instance (Show a, Ord a, Num a) => Show (Complex a) where
  show (Complex r i) 
    | i == 0 = show r
    | r == 0 = show i ++ "i"
    | i < 0 = show r ++ show i ++ "i"
    | otherwise = show r ++ "+" ++ show i ++ "i"

instance (Floating a, Eq a) => Num (Complex a) where
  Complex r1 i1 + Complex r2 i2 = Complex (r1 + r2) (i1 + i2)
  Complex r1 i1 * Complex r2 i2 = 
    Complex (r1 * r2 - i1 * i2) (r1 * i2 + r2 * i1)
  fromInteger x = Complex (fromInteger x) 0
  negate (Complex r i) = Complex (negate r) (negate i)
  abs c = Complex (absComplex c) 0
  signum c@(Complex r i) 
    | c == 0 = 0
    | otherwise = Complex (r / a) (i / a)
     where a = absComplex c

-- auxiliary functions must be defined outside 
-- the class instantiation
absComplex (Complex r i) = sqrt (r^2 + i^2)

tests = do 
  putStrLn (show ((Complex 0 1)^2))
  putStrLn (show (2 + 5 :: Complex Float))
  putStrLn (show (abs (Complex 1 3) :: Complex Float))
  putStrLn (show (abs (Complex 1 3) :: Complex Double))
  putStrLn (show (2 * Complex 7 2.5))