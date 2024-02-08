module Rat(Rat, createRat, numerator, denominator) where

data Rat = Rat Integer Integer
  deriving Eq -- sound because of invariant

instance Show Rat where -- no normalization required
  show (Rat n d) = if d == 1 then show n else show n ++ "/" ++ show d

numerator (Rat n d) = n
denominator (Rat n d) = d

normalize (Rat n d) 
  | d < 0 = normalize (Rat (- n) (- d))
  | otherwise = Rat (n `div` g) (d `div` g) where
      g = gcd n d
createRat n d = normalize $ Rat n d

instance Ord Rat where
  Rat n1 d1 <= Rat n2 d2 = n1 * d2 <= n2 * d1

instance Num Rat where 
  -- no normalization required, implicit
  negate (Rat n d) = Rat (-n) d 

  -- multiplication requires normalization to obey invariant 
  Rat n1 d1 * Rat n2 d2 = createRat (n1 * n2) (d1 * d2) 

  Rat n1 d1 + Rat n2 d2 = createRat (n1 * d2 + n2 * d1) (d1 * d2)

  fromInteger x = Rat x 1

  abs (Rat n d) = Rat (abs n) d

  signum (Rat n d) = fromInteger (signum n)
  
