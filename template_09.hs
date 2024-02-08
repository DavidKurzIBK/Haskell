{-
because of exercise 2 (the import of LeanCheck), start ghci via stack as follows:

stack ghci template_09.hs

(or put the Haskell code for exercise 2 into comments)
-}

-- Exercise 2
import Test.LeanCheck
import Tree


-- TODO: remove once imports are in place
--data Tree a = Tree

-- TODO: define Listable instance
instance Listable a => Listable (Tree a) where
	tiers = cons0 X \/ cons3 Node

prop_splitAtLevel_implies_fillXs ::
Int -> Tree Int -> Tree Int -> [Tree Int] -> Bool
	
prop_splitAtLevel_implies_fillXs i t s ss =
	splitAtLevel i t == (s, ss) ==> fillXs s ss == (t,[]) 


-- Exercise 1

-- scope defines which names of variables, functions, types, . . . are visible at a given program position
-- scope of a variable is determined by its binding

radius = 10  -- global radius

computeVolume :: Double -> Double
computeVolume rad = (4/3)*pi*rad^3

operationA :: Double -> Double
operationA radius =  computeVolume radius
-- radius refers to a local parameter within operationA function not a global radius of 10!!
-- This local parameter shadows the global variable radius. The radius within the function refers to the local parameter, not the global one.

--ghci> operationA radius
--4188.790204786391

--ghci> operationA 10
--4188.790204786391

--ghci> operationA 30
--113097.33552923254

--ghci> operationA
-- <interactive>:6:1: error:
--    * No instance for (Show (Double -> Double))
--        arising from a use of `print'
--        (maybe you haven't applied a function to enough arguments?)
--    * In a stmt of an interactive GHCi command: print it


operationB :: Double
operationB  = computeVolume radius
-- radius used in computeVolume function refers to global variable 
-- cause there is no local parameter named radius within the scope of operationB.

--ghci> operationB 30
-- <interactive>:4:1: error:
--    * Couldn't match expected type `t0 -> t' with actual type `Double'
--   * The function `operationB' is applied to one value argument,
--        but its type `Double' has none
--      In the expression: operationB 30
--      In an equation for `it': it = operationB 30
--   * Relevant bindings include it :: t (bound at <interactive>:4:1)

--ghci> operationB
--4188.790204786391


operationC :: Double -> Double
operationC = computeVolume
-- parameter of operationC not explicitly named
-- function defined as computeVolume. 
-- parameter in computeVolume refer to parameter of operationC, which is implicitly there, is local to operationC and shadows global variable radius.

--ghci> operationC radius
--4188.790204786391

--ghci> operationC 40
--268082.573106329

--ghci> operationC
-- <interactive>:4:1: error:
--    * No instance for (Show (Double -> Double))
--        arising from a use of `print'
--        (maybe you haven't applied a function to enough arguments?)
--    * In a stmt of an interactive GHCi command: print it




--reverseList :: [a] -> [a]
--	reverseList xs_1 =
--		let reverseListAux xs_2 ys_1 = case xs_3 of 
--				(x_1:xs_4) -> reverseListAux xs_5 (x_2:ys_2)
--				_ -> ys_3
--		in reverseListAux xs_6 []

--Use xs_2 instead of xs_3 in the case expression.
--Use xs_4 instead of xs_5 in the recursive call.
--Use x_1:ys_1 instead of x_2:ys_2 in the recursive call.
--Remove the unnecessary variables ys_2, ys_3, and xs_6 as they were not needed in this context.

reverseList :: [a] -> [a]
reverseList xs_1 = 
	let reverseListAux xs_2 ys_1 = case xs_2 of 
		(x_1:xs_4) -> reverseListAux xs_4 (x_1:ys_1)
		_ -> ys_1
	in reverseListAux xs_1 []
	
	
	


squareRootTwo :: Double -> Integer -> Double
squareRootTwo guess n
    | n == 0 = guess
    | otherwise = squareRootTwo ((guess + 2/guess) / 2) (n-1)

squareRootTwoA :: Double -> Integer -> Double
squareRootTwoA guess n
    | n == 0 = guess
    | otherwise = squareRootTwoA ((guess + 2/guess) / 2) (n-1) where n=n

squareRootTwoB :: Double -> Integer -> Double
squareRootTwoB guess n
    | n == 0 = guess
    | otherwise = let n = n-1 in squareRootTwoB ((guess + 2/guess) / 2) n

