
-- The following imports are solely used for testing and time measurements
import System.CPUTime
import System.Timeout
import Text.Printf

{- Exercise 1 -}
type Vector a = [a]		-- alias for list of elements of type a
type Matrix a = [[a]]	-- alias for a list of lists of elements 

-- function indices: takes vector v and uses range [0 .. length v - 1] to generate list of indices from 0 to one less than length of vector.
indices :: Vector a -> [Int]
indices v = [0 .. length v - 1]


-- Task 1.1
-- takes vector v and negates each element of the vector using a list comprehension and indexing
negateVecSlow :: Num a => Vector a -> Vector a
negateVecSlow v = [- v !! i | i <- indices v]

-- map: higher-order function, applies function to each element of list, returning new list. type signature: ""map :: (a -> b) -> [a] -> [b]""
-- negate: unary function, negates its numeric argument. type signature: ""negate :: Num a => a -> a""
negateVec :: Num a => Vector a -> Vector a
negateVec v = map negate v



-- Task 1.2
-- two vectors v and w and adds corresponding elements using a list comprehension and indexing
vecAddSlow :: Num a => Vector a -> Vector a -> Vector a
vecAddSlow v w = [v !! i + w !! i | i <- indices v]

-- zipWith function: higher-order function: combines elements of two vectors using specified binary function, here elementwise addition (+).
-- signature: ""zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]""
vecAdd :: Num a => Vector a -> Vector a -> Vector a
vecAdd v w = zipWith (+) v w 



-- Task 1.3
--  scalar product of two vectors v and w using a list comprehension and indexing
scalarProductSlow :: Num a => Vector a -> Vector a -> a
scalarProductSlow v w = sum [v !! i * w !! i | i <- indices v]

-- zipWith to perform elementwise multiplication and then sum of products
scalarProduct :: Num a => Vector a -> Vector a -> a
scalarProduct v w = sum(zipWith (*) v w)
-- scalarProduct v w = sum [x * y | (x, y) <- zip v w]



-- Task 1.4
transposeSlow :: Matrix a -> Matrix a
transposeSlow a =
  let rowIdcs = indices a
      colIdcs = indices (head a)
   in [[a !! i !! j | i <- rowIdcs] | j <- colIdcs]

transpose :: Matrix a -> Matrix a
transpose a = transpose a
-- transpose m = [map (!! j) m | j <- indices (head m)]



-- Task 1.5
matMultSlow :: Num a => Matrix a -> Matrix a -> Matrix a
matMultSlow a b =
  let n = indices a
      m = indices (head b)
   in [[scalarProductSlow (a !! i) (map (!! j) b) | j <- m] | i <- n]

matMult :: Num a => Matrix a -> Matrix a -> Matrix a
matMult a b = [[sum $ zipWith (*) row col | col <- transpose b] | row <- a]
-- matMult a b = [map (\i -> scalarProduct (a !! i) (map (!! i) b)) [0 .. length (head b) - 1] | 0 .. length a - 1]



{- Exercise 2 -}
-- Task 2.1

--Summary on Fold: 
-- • a fold-function can be defined for most datatypes
--fold replaces constructors by functions
-- • after having programmed fold for an individual datatype, one can define many recursive
--algorithms just by suitable invocations of fold


-- foldr and foldl: higher-order functions in context of lists or other data structures to also apply binary functions 

-- Signature: ""foldr :: (a -> b -> b) -> b -> [a] -> b""
-- foldr f e [] = e
-- foldr f e (x : xs) = x `f` (foldr f e xs)
-- captures structural recursion on lists
-- • e result of the base case
-- • f how compute result given first list element and the recursive result
-- foldr f e [x_1, x_2, x_3, x_4] = x_1 `f` (x_2 `f` (x_3 `f` (x_4 `f` e)))
-- f is function applied in each node, all x_s are left leafes and e ist the last leaf right. 


-- Signature: ""foldl :: (b -> a -> b) -> b -> [a] -> b""
-- foldl f e [x_1, x_2, x_3] = ((e `f` x_1) `f` x_2) `f` x_3
-- is just the mirrored tree like foldr!! 


-- Task 2.2
insertionSortRec :: Ord a => [a] -> [a]
insertionSortRec [] = []
insertionSortRec (x : xs) = insert (insertionSortRec xs)
  where
    insert [] = [x]
    insert (y : ys)
      | x < y = x : y : ys
      | otherwise = y : insert ys

insertionSortFold :: Ord a => [a] -> [a]
insertionSortFold = undefined

-- Task 2.3

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)
-- binary tree either be a Leaf (representing an empty tree) or "|" a Node containing value of type a and two subtrees (left and right).

exampleTree =
  let n x = Node Leaf x Leaf
   in Node (Node (n 1) 2 (n 3)) 4 (Node Leaf 5 (Node (n 6) 7 Leaf))
-- using the defined ""Node"" constructor. 
-- n function creates a node with a given value and empty left and right subtrees

foldt :: (b -> a -> b -> b) -> b -> Tree a -> b
foldt _ e Leaf = e  
-- foldt f e (Leaf x) = e
foldt f e (Node l x r) = f (foldt f e l) x (foldt f e r)
-- foldt: higher-order function performs a fold on a binary tree. 
-- takes folding function f, initial accumulator e, and a Tree a. 
-- folding function f takes an accumulator, a value, and another accumulator, and recursively applies it to the left and right subtrees.
-- rekursive Funktion foldt(): binärer Baum ist Eingabeparameter; Baum leer -> wert null??  und nur ein konten, dann e = wert von Wurzelknoten
-- Andernfalls: Wert des Wurzelknotens an die Liste und ruft dann foldt() für linken und rechten Unterbäume auf und addiert alle werte der Blätter 
-- und folgenden Knoten auf.


height :: Tree a -> Int
-- height (Leaf x) = 0 
height = foldt (\h _ h' -> 1 + max h h') 0
-- height (Node l x r) = 1 + max (height l) (height r)
-- initializes fold with a function taking height of the left subtree h, the current node value _, and the height of the right subtree h'. It returns 1 + max h h'
-- wurzelknoten hat height = 1, danach wird jeweils die größte (tiefe = höhe des jeweiligen linken oder rechten subtrees ermittelt. 
-- wenn kein foldt init, dann null ausgebe. 


flatten :: Tree a -> [a]
flatten = foldt (\l x r -> l ++ [x] ++ r) []
-- flatten (Leaf x) = [x]
-- flatten (Node l x r) = flatten l ++ [x] ++ flatten r
-- convert tree into list by traversing it in-order fashion by concatenating firstly flattend left subtree l, 
-- then current node value [x], and finally flattend right subtree r to a list. 


mirror :: Tree a -> Tree a
--mirror (Leaf x) = Leaf x
--mirror (Node l x r) = Node (mirror r) x (mirror l)
mirror = foldt (\l x r -> Node r x l) Leaf
-- mirrors is swapping left and right subtrees at each node one time; 
-- foldt function with a folding function that constructs a new node with the right and left subtrees swapped



mapTree :: (a -> b) -> Tree a -> Tree b
--mapTree f (Leaf x) = Leaf (f x)
--mapTree f (Node l x r) = Node (mapTree f l) (f x) (mapTree f r)
mapTree f = foldt (\l x r -> Node l (f x) r) Leaf
-- applies given function f to each element in the tree, using foldt with a folding function that applies a specified function to the current node value.



showTree :: Show a => Tree a -> String
showTree = foldt (\l x r -> concat [indent, show x, "\n", drawBranch '/', l, drawBranch '\\', r]) ""
  where
    indent = "   "

    drawBranch _ Leaf = ""
    drawBranch c t = concat [indent, [c], "\n", showTree t]

-- showTree = foldt (\l x r -> concat [l, indent, show x, "\n", r]) ""
--   where
--     indent = "   "
-- convert tree into string representation. 
-- uses foldt that concatenates the current node value x with the indented left and right subtrees.


{- Tests -}

-- Tests for Exercise 1
tests1 = do
  putStrLn "Testing functional correctness of vector and matrix operations"
  testNeg (generateVec 5)
  testNeg testvecV
  testAdd (generateVec 5) (generateVec 5)
  testAdd testvecV testvecW
  testScalar (generateVec 5) (generateVec 5)
  testScalar testvecV testvecW
  testTranspose (generateMat 5)
  testTranspose testmatA
  testTranspose testmatB
  testMult (generateMat 4) (generateMat 4)
  testMult testmatA testmatB
  putStrLn $ replicate 80 '='
  putStrLn "Testing efficiency of vector and matrix operations"
  testTiming

-- Tests for Exercise 2
tests2 = sequence_ [testFoldt, testHeight, testFlatten, testMirror, testMapTree, testShowTree]

generateVec n = [1 .. n]

generateMat n = replicate n [1 .. n]

testvecV = [3, 1, -20, 15]

testvecW = [490, -2, 3, -5]

testmatA = [[1, 2, 3], [4, 5, 6]]

testmatB = [[7, 10, 11, 12], [8, 13, 14, 15], [9, 16, 17, 18]]

testEq a b s t
  | a == b = putStrLn $ t ++ " (OK)"
  | otherwise = putStrLn $ t ++ " (FAILED)\n" ++ "expected:\n" ++ s a ++ "\ncomputed:\n" ++ s b

showMat :: Show a => Matrix a -> String
showMat a = ("[" ++) $ drop 3 $ concatMap ((",\n " ++) . show) a ++ "]"

testNeg v = testEq (negateVecSlow v) (negateVec v) show ("testing: negateVec " ++ show v)

testAdd v w = testEq (vecAddSlow v w) (vecAdd v w) show ("testing: addVec " ++ show v ++ " " ++ show w)

testScalar v w = testEq (scalarProductSlow v w) (scalarProduct v w) show ("testing: scalarProduct " ++ show v ++ " " ++ show w)

testTranspose a = testEq (transposeSlow a) (transpose a) showMat ("testing: transpose " ++ show a)

testMult a b = testEq (matMultSlow a b) (matMult a b) showMat ("testing: matMult " ++ show a ++ " " ++ show b)

timedInt x
  | x == 0 = return True
  | otherwise = return False

timedMat a = timedInt (sum (map sum a))

timedVec v = timedInt (sum v)

timedCompute s i = do
  let to = 10 * 10 ^ 6 -- use 10 seconds timeout
  putStrLn s
  start <- getCPUTime
  res <- timeout to i
  case res of
    Nothing -> putStrLn "*** timeout after 10 seconds"
    Just _ -> do
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10 ^ 12) :: Double
      printf "Computation time: %0.3f sec\n" diff

testTiming = do
  let negSlow n = timedCompute ("n = " ++ show n) (timedVec (negateVecSlow (generateVec n)))
  let neg n = timedCompute ("n = " ++ show n) (timedVec (negateVec (generateVec n)))
  let addSlow n = timedCompute ("n = " ++ show n) (timedVec (vecAddSlow (generateVec n) (generateVec n)))
  let add n = timedCompute ("n = " ++ show n) (timedVec (vecAdd (generateVec n) (generateVec n)))
  let scalarSlow n = timedCompute ("n = " ++ show n) (timedInt (scalarProductSlow (generateVec n) (generateVec n)))
  let scalar n = timedCompute ("n = " ++ show n) (timedInt (scalarProduct (generateVec n) (generateVec n)))
  let transSlow n = timedCompute ("n = " ++ show n) (timedMat (transposeSlow (generateMat n)))
  let trans n = timedCompute ("n = " ++ show n) (timedMat (transpose (generateMat n)))
  let multSlow n = timedCompute ("n = " ++ show n) (timedMat (matMultSlow (generateMat n) (generateMat n)))
  let mult n = timedCompute ("n = " ++ show n) (timedMat (matMult (generateMat n) (generateMat n)))
  putStrLn "On our reference machine, each non-slow test needs at most 1 second."
  putStrLn "Your implementations should at least be faster than the slow ones"
  putStrLn "(for the same value of n)."
  putStrLn $ replicate 80 '='
  putStrLn "Negation Slow"
  mapM_ negSlow [1000, 10000, 100000]
  putStrLn $ replicate 80 '-'
  putStrLn "Negation"
  mapM_ neg [100000, 1000000, 10000000]
  putStrLn $ replicate 80 '='
  putStrLn "Addition Slow"
  mapM_ addSlow [1000, 10000, 100000]
  putStrLn $ replicate 80 '-'
  putStrLn "Addition"
  mapM_ add [100000, 1000000, 10000000]
  putStrLn $ replicate 80 '='
  putStrLn "Scalar Product Slow"
  mapM_ scalarSlow [1000, 10000, 100000]
  putStrLn $ replicate 80 '-'
  putStrLn "Scalar Product"
  mapM_ scalar [100000, 1000000, 10000000]
  putStrLn $ replicate 80 '='
  putStrLn "Transpose Slow"
  mapM_ transSlow [200, 400, 800]
  putStrLn $ replicate 80 '-'
  putStrLn "Transpose"
  mapM_ trans [800, 1600, 3200]
  putStrLn $ replicate 80 '='
  putStrLn "Matrix Multiplication Slow"
  mapM_ multSlow [50, 100, 150]
  putStrLn $ replicate 80 '-'
  putStrLn "Matrix Multiplication"
  mapM_ mult [150, 200, 300]

------------------
--- Ex 2 Tests ---
------------------

exampleTree2 = Leaf

exampleTree3 = Node Leaf 1 (Node Leaf (-3) Leaf)

testFoldt = do
  testFoldAux 28 (\l x r -> l + x + r) "(\\l x r -> l + x + r)" 0 (exampleTree :: Tree Integer)
  testFoldAux 1 (\_ _ _ -> 5) "(\\_ _ _ -> 5)" 1 (exampleTree2 :: Tree Double)
  testFoldAux 8 (\l x r -> l - x + r) "(\\l x r -> l - x + r)" 2 exampleTree3
  where
    testFoldAux expected f fstr e t = testEq expected (foldt f e t) show ("testing: foldt " ++ fstr ++ " " ++ show e ++ " $ " ++ show t)

testHeight = mapM_ (\(exp, t) -> testEq exp (height t) show ("testing: height " ++ show t)) [(4, exampleTree), (0, exampleTree2), (2, exampleTree3)]

testFlatten = mapM_ (\(exp, t) -> testEq exp (flatten t) show ("testing: flatten " ++ show t)) [([1, 2, 3, 4, 5, 6, 7], exampleTree), ([], exampleTree2), ([1, -3], exampleTree3)]

testMirror =
  mapM_
    (\(exp, t) -> testEq exp (show $ mirror t) show ("testing: mirror " ++ show t))
    [ ("Node (Node (Node Leaf 7 (Node Leaf 6 Leaf)) 5 Leaf) 4 (Node (Node Leaf 3 Leaf) 2 (Node Leaf 1 Leaf))", exampleTree),
      ("Leaf", exampleTree2),
      ("Node (Node Leaf (-3) Leaf) 1 Leaf", exampleTree3)
    ]

testMapTree = do
  testMapTreeAux "Node (Node (Node Leaf False Leaf) True (Node Leaf False Leaf)) True (Node Leaf False (Node (Node Leaf True Leaf) False Leaf))" even "even" (exampleTree :: Tree Integer)
  testMapTreeAux "Leaf" (const 10) "(const 10)" (exampleTree2 :: Tree String)
  testMapTreeAux "Node Leaf (-1) (Node Leaf 3 Leaf)" negate "negate" exampleTree3
  where
    testMapTreeAux expected (f :: a -> b) fstr t = testEq expected (show $ (mapTree f t :: Tree b)) show ("testing: mapTree " ++ fstr ++ " $ " ++ show t)

testShowTree =
  mapM_
    (\(exp, t) -> testEq exp (showTree t) ("testing: showTree $ " ++ show t))
    [ ("      1\n    /\n   2\n    \\\n      3\n /\n4\n \\\n   5\n    \\\n         6\n       /\n      7\n", exampleTree),
      ("", exampleTree2),
      ("1\n \\\n   -3\n", exampleTree3)
    ]
  where
    testEq expected res prefx
      | expected == res = do
          putStrLn $ prefx ++ " (OK)"
      | otherwise = do
          putStrLn $ replicate 80 '='
          putStrLn $ prefx ++ " (FAILED)"
          putStrLn $ "expected:\n" ++ expected
          putStrLn $ "computed:\n" ++ res
          putStrLn $ replicate 80 '='