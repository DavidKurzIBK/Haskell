

{- Exercise 1 -}

data Rat = Rat Integer Integer


{- Question 1
simple representation of rational numbers using a data type Rat with two integer fields (numerator and denominator).
implement function normaliseRat normalizes rational number by dividing (div) numerator and denominator by their greatest common divisor (gcd). 
function createRat that takes two integers and returns a normalized Rat
conditional statement handles case when both the numerator and denominator are negative
createRat function takes two integers, converts them to Integer type
-}
normaliseRat :: Rat -> Rat
normaliseRat (Rat n d)
 | d == 0 = error "Denominator must be positive"
 | otherwise =
    if n < 0 && d < 0
      then Rat (div (-n) (gcd n d)) (div (-d) (gcd n d)) -- example: div 7 3 returns 2 because the division of 7 by 3 is 2 with a remainder. 

      else Rat (div n (gcd n d)) (div d (gcd n d))  -- example: The gcd 8 12 would be 4. The normalized rational number would then be Rat (div 8 4) (div 12 4), which simplifies to Rat 2 3.
	  
	  
createRat :: Integer -> Integer -> Rat
createRat n d = normaliseRat(Rat (toInteger n)(toInteger d))



{- Question 2
Type Class Instances
• many types are instances of Eq and Ord
• Eq Int meaning: Int is an instance of Eq

(==) "overload" compare two Rat values for equality. convert numerator and denominator to Double using fromIntegral and then checks resulting floating-point values are equal. 
allows for approximate equality comparison, considering potential rounding errors when dealing with floating-point numbers.
(<) and (<=) operators similar to the Eq instance, converts numerators and denominators to Double and performs corresponding floating-point comparisons.
-}

instance Eq Rat where
	(==)(Rat n1 d1)(Rat n2 d2) = (fromIntegral n1 / fromIntegral d1 ) == (fromIntegral n2 / fromIntegral d2)

instance Ord Rat where
	(<)(Rat n1 d1)(Rat n2 d2) = (fromIntegral n1 / fromIntegral d1) < (fromIntegral n2 / fromIntegral d2)
	(<=)(Rat n1 d1)(Rat n2 d2) = (fromIntegral n1 / fromIntegral d1) <= (fromIntegral n2 / fromIntegral d2)	

-- instance: specifies how these functions are implemented for a particular type.



-- Question 3
instance Show Rat where
 show(Rat n d)
  | d == 0 = error "Denominator must be positive"
  | n < 0 && d < 0 = show (-n) ++ "/" ++ show (-d)
  | d < 0 = show(-n)  ++ "/" ++ show(-d)
  | d == 1 = show n
  | otherwise = show n ++ "/" ++ show d



-- Question 4
instance Num Rat where
	negate(Rat n d) = normaliseRat(Rat(-n)d) -- negates n while d unchanged. result is normalized using normaliseRat function.
	abs(Rat n d) = normaliseRat(Rat(abs n)(abs d)) -- takes bsolute value of both the numerator n and denominator d
	fromInteger x = Rat(fromInteger x) 1 -- setting the integer as the numerator and 1 as the denominator. 
	(*)(Rat n1 d1)(Rat n2 d2) = normaliseRat(Rat(n1*n2)(d1*d2)) -- multiplies numerators and denominators separately and creates new Rat value
	(+)(Rat n1 d1)(Rat n2 d2) = normaliseRat(Rat(n1*d2+n2*d1)(d1*d2)) --  cross-multiplies numerators and denominators, adds results, and creates a new Rat value
	signum(Rat n1 d1) = if normaliseRat(Rat n1 d1) > 0 then 1 else -1 -- if normalized Rat is greater than 0, it returns 1; otherwise returns -1.





{- Exercise 2 
-}


-- type class with 2 functions: binop (binary operation function)
class MonoidC a where
  binop :: a -> a -> a
  neutral :: a


-- Question 1

-- define MonoidC instances for Double and Integer (addition) and Lists (append)
-- Monoid instance for Double (using addition)
instance MonoidC Double where
  binop = (+)
  neutral = 0.0

-- Monoid instance for Integer (using addition)
instance MonoidC Integer where
  binop = (+)
  neutral = 0

-- Monoid instance for lists (using append)
instance MonoidC [a] where
  binop = (++)
  neutral = []

-- Using Double instance
--let resultDouble = binop 2.0 3.0       -- resultDouble will be 5.0

-- Using Integer instance
--let resultInteger = binop 2 3          -- resultInteger will be 5

-- Using List instance
--let resultList = binop [1, 2] [3, 4]   -- resultList will be [1, 2, 3, 4]





{-Question 2
data type Tally with single constructor PM that takes a String argument. 
defines a function normalise that takes a String as input and returns a normalized version of that string
the single constructor PM is called a "newtype."
Newtypes used to provide new name for an existing type, often without changing the underlying representation or behavior of the type
-}

data Tally = PM String

normalise :: String -> String
normalise "" = ""  -- base case for returning an empty string
normalise ('+':'-':rest) = normalise rest  -- remove "+-" sublist and recursively calls normalise on the rest of the string.
normalise ('-':'+':rest) = normalise rest  -- remove "-+" sublist and recursively calls normalise on the rest of the string.
normalise (x:rest) = x : normalise rest    -- keep other characters as is
  
  
  
  
-- Question 3
-- make Tally instance of Eq, Show and MonoidC
-- uses normalise function to compare normalized strings inside the PM constructors. 
-- Two Tally values are considered equal if their normalized string representations are equal.
instance Eq Tally where
  (PM xs) == (PM ys) = normalise xs == normalise ys

instance Show Tally where
  show (PM xs) = xs -- show function is used to convert a Tally value to its string representation.

instance MonoidC Tally where
  binop (PM xs) (PM ys) = PM (normalise (xs ++ ys)) -- binop function takes 2 Tally values, extracts strings inside the PM constructors, 
  -- concatenates them, normalizes the result using the normalise function, and wraps the normalized result in a new Tally.
  neutral = PM "" --  returns the neutral element for the Tally monoid, which is empty Tally (a PM with an empty string)
  
  
  
  

{- Question 4
signature MonoidC a => [a] -> a indicates that combine works for any type a that is an instance of the MonoidC type class. 
It takes list of values of type a and returns a single value of type a.
1. foldr function, which is a right-fold (right-associative fold) over the list.
2. binop function from the MonoidC instance for type a is used as the folding function. It combines each element of the list with the accumulated result.
3. neutral value : serves as the initial accumulator for the fold.
-}
combine :: MonoidC a => [a] -> a
combine = foldr binop neutral



{-combine :: [Integer] -> Integer:

takes a list of integers and computes their sum using the addition operation.
combines the elements of the list using the binary operation of addition (+), and the neutral element is 0. 
The result is the sum of all the integers in the list.

combine :: [String] -> String:

takes a list of strings and concatenates them.
combines the elements of the list using the binary operation of string concatenation (++), and the neutral element is the empty string "". 
The result is the concatenation of all the strings in the list.

Difference between combine [[1,3,7]] and combine [1,3,7]:
combine [[1,3,7]]: takes a list containing a single element, which is itself a list [1,3,7]. The result is the concatenation of the inner list, resulting in [1,3,7].
combine [1,3,7]: takes a list of three integers and computes their sum, resulting in the integer 11.
In summary, the difference lies in the type of operation performed and the structure of the input list.

Difference between combine [PM "++--+", PM "+---+", PM "---+"] and combine ["++--+", "+---+", "---+"]:
combine [PM "++--+", PM "+---+", PM "---+"]: takes a list of Tally values and combines them using the binop operation defined for Tally instances. 
The result is a Tally value obtained by concatenating and normalizing the strings inside the PM constructors.
 -}
  
  
{- executable Tests are only available for Exercise 1; 
   for Exercise 2, you first have to add the class instantions and can then manually test the following: 
   combine [1 .. 100 :: Integer] = 5050
   combine (map (\x -> [x]) ['a' .. 'z']) = "abcdefghijklmnopqrstuvwxyz"
   combine [PM "++--+", PM "+---+", PM "---+"] = PM "--"
   combine ["++--+", "+---+", "---+"] = "++--++---+---+"
   normalise "++--+++-++++" = "++++++" 
   PM "++--+++-++++" == PM "++++++" 
   PM "+--+" == PM ""
   PM "+--+" /= PM "+-+"
-}
testRats = do
  check
    "normaliseRat"
    "[True,True,True,False]"
    ( map
        ( \(r1, r2) -> case (normaliseRat r1, normaliseRat r2) of
            (Rat n1 d1, Rat n2 d2) -> (n1, d1) == (n2, d2)
        )
        [(Rat (-1) (-2), Rat 2 4), (Rat (-3) 7, Rat 6 (-14)), (Rat 0 3, Rat 0 4), (Rat 1 3, Rat (-1) 3)]
    )
  check "createRat" "-1/2" (createRat 5 (-10))
  check "Equality on Rats" "[True,False,False]" [Rat 3 5 == Rat (-9) (-15), Rat 3 5 == Rat 5 3, Rat 3 5 == Rat 3 (-5)]
  check
    "Order on Rats"
    "[True,False,True]"
    [Rat 3 5 < Rat 3 4, Rat 2 4 < Rat 2 4, Rat (-5) 3 < Rat 10 1]
  check "Show on Rats" "[3,4/5,-1/2]" [Rat 3 1, Rat 4 5, Rat 1 (-2)]
  check
    "Num on Rats"
    "[1/2,7/12,3/4,-1]"
    [3 * Rat 1 6, Rat 1 3 + Rat 1 4, abs (Rat (-3) 4), signum (negate (Rat 3 4))]

check name e c = do
  putStr ("*** " ++ name ++ ": ")
  if show c == e
    then putStrLn "OK"
    else putStrLn ("ERROR; expected '" ++ e ++ "', but found '" ++ show c ++ "'")