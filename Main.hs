module Main where

import Control.Exception (AssertionFailed, assert, evaluate, try)
import Data.List qualified as D (intersect, union, (\\))
import ListSet
import SetMore
import System.CPUTime
import System.Timeout
import Text.Printf

{- Build using `ghc --make Main.hs` -}
main :: IO ()
main = do
  putStrLn "*** Testing base set interface implementation ***"
  testBaseSetInterface
  putStrLn "\n*** Testing SetMore implementation ***"
  testSetMoreInterface
  putStrLn "\n*** Testing timing ***"
  testTiming

testBaseSetInterface :: IO ()
testBaseSetInterface = do
  hAssert (not $ member 5 empty) "the empty set should have no members"
  -- Deleting an element from the empty set should be allowed
  let e = delete 5 empty
  -- Insert some (duplicated) elements
  let insertedEls = concat $ replicate nDups setElems
  let setWithEls = foldr insert e insertedEls
  hAssert (all (`member` setWithEls) setElems) "inserted elements should be in the set"
  -- Modify set elements with foldSet
  let foldedSet = foldSet (\el xs -> modifyElems el : xs) [] setWithEls
  -- Ensure that original and new elements are in modifiedSet
  hAssert (all (`elem` foldedSet) modifiedElems) "set modification with foldSet"
  -- Remove a single element
  let delSet = delete (head setElems) setWithEls
  hAssert (not $ member (head setElems) delSet) "deleted element should no longer be a set member"
  -- Remove all set elements
  let emptySet = foldr delete delSet setElems
  hAssert (all (\el -> not $ member el emptySet) setElems) "deleted elements should no longer be in set"
  where
    nDups = 3 -- Number of duplicate elements to insert
    setElems = [1 .. 10]
    modifyElems = (+ 10) -- Function modify each set element
    modifiedElems = map modifyElems setElems

testSetMoreInterface :: IO ()
testSetMoreInterface = do
  -- Test empty set cases
  let e = empty :: Set Int
  hAssert (isEmpty e) "the empty set should satisfy isEmpty"
  hAssert (isEmpty $ e `union` empty) "the union of empty sets should satisfy isEmpty"
  hAssert (isEmpty $ e `intersection` empty) "the intersection of empty sets should satisfy isEmpty"
  -- Test union and intersection
  let setX = foldr insert empty xs
  let setY = foldr insert empty ys
  let xsUnionYs = xs `D.intersect` ys
  let xsIntersectYs = xs `D.intersect` ys
  hAssert (all (`member` (setX `union` setY)) xsUnionYs) "elements in the union of elements should be in the set union"
  hAssert (all (`member` (setX `intersection` setY)) xsIntersectYs) "elements in the intersection of elements should be in the set intersection"
  hAssert (all (\el -> not $ el `member` (setX `intersection` setY)) (xsUnionYs D.\\ xsIntersectYs)) "elements not in the intersection of elements should not be in the set intersection"
  where
    nDups = 3 -- Number of duplicate elements to insert
    maxSetEl = 10
    xs = [1 .. maxSetEl]
    ys = [3, 6 .. maxSetEl * 2]

timedInt s x
  | x == 0 = return s
  | otherwise = return s

-- Force reading entire set
timedSet :: (Monad m, Eq a, Num a) => Set a -> m (Set a)
timedSet s = timedInt s $ foldSet (+) 0 s

-- Force reading entire set
timedBool :: (Monad m) => [Bool] -> m [Bool]
timedBool b = timedInt b $ foldr (\b acc -> if b then acc + 1 else acc) 0 b

testTiming :: IO ()
testTiming = do
  let smallSetSize = 500
  let largeSetSize = 2000
  let smallestSets = [(smallSetSize, 1), (smallSetSize, 10), (smallSetSize, 15)]
  let smallerSets = [(smallSetSize, 1), (smallSetSize, 10), (smallSetSize, 50)]
  let largerSets = [(largeSetSize, 1), (largeSetSize, 10), (largeSetSize, 100)]
  let genSetEls nEls nDups = concat $ replicate nDups [nEls `div` 2 .. nEls `div` 2 + nEls] :: [Int]
  let tSetInsert (nEls, nDups) = timeF nEls nDups $ timedSet $ foldr insert empty (genSetEls nEls nDups)
  let tFoldSet ((nEls, nDups), s) = timeF nEls nDups $ timedSet $ foldSet (\el s -> if el == -1 then empty else s) empty s
  let tSetMember ((nEls, nDups), s) = timeF nEls nDups $ timedBool $ map (`member` s) (genSetEls nEls nDups)
  let tSetDeletion ((nEls, nDups), s) = timeF nEls nDups $ timedSet $ foldr delete s (genSetEls nEls nDups)
  putStrLn "All tests take under 60 seconds on our test machine"
  putStrLn "If you get an \"Out of memory\" error or time 0.0 for too many tests, then try adjusting smallestSetSize or smallSetSize or largeSetSize"
  skipL
  putStrLn "Set insertion"
  -- Generate smaller and larger sets for testing functions which take different amounts of time
  smallerSs <- mapM tSetInsert smallerSets
  largerSs <- mapM tSetInsert largerSets
  skipL
  putStrLn "foldSet"
  mapM_ tFoldSet (zip largerSets largerSs)
  skipL
  putStrLn "Set membership"
  mapM_ tSetMember (zip smallestSets largerSs)
  skipL
  putStrLn "Set deletion"
  mapM_ tSetDeletion (zip smallestSets largerSs)
  skipL
  where
    -- Timing helper functions
    skipL = putStrLn $ replicate 80 '='
    printTestParams nEls nDups = show nEls ++ " elements " ++ show nDups ++ " duplicate(s) "
    timeF nEls nDups = timedCompute $ printTestParams nEls nDups

-----------------------
-- Helper functions
-----------------------
hAssert :: Bool -> String -> IO ()
hAssert bool msg = do
  result <- try (assert bool (\s -> putStrLn ("* OK: " ++ s)) msg) :: IO (Either AssertionFailed ())
  case result of
    Left _ -> putStrLn $ "* ERROR: " ++ msg -- Catch exception
    Right _ -> return ()

timedCompute :: String -> IO a -> IO a
timedCompute s i = do
  let to = 10 * 10 ^ 6 -- use 10 seconds timeout
  putStr s
  start <- getCPUTime
  val <- i
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 9) :: Double -- Time in msec
  printf "computation time: %0.1f msec\n" diff
  return val
