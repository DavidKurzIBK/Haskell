


data Expr = Variable String | Number Integer | Add Expr Expr | Negate Expr deriving Show
data Assign = Empty | Assign String Integer Assign deriving Show

exampleExpr = Negate (Add (Variable "x") (Negate (Add (Variable "y") (Number 3))))
exampleAssign = Assign "x" 5 (Assign "y" 12 Empty)

ite True x y = x
ite False x y = y


value :: String -> Assign -> Integer
value var Empty = error ("Variable not found: " ++ var)
value var (Assign name val rest)
    | var == name = val
    | otherwise = value var rest



eval :: Assign -> Expr -> Integer
eval _ (Number n) = n
eval assign (Variable var) = value var assign
eval assign (Add expr1 expr2) = eval assign expr1 + eval assign expr2
eval assign (Negate expr) = -eval assign expr 



containsVar :: String -> Assign -> Bool
containsVar _ Empty = False
containsVar var (Assign name _ rest) 
    | var == name = True
    | otherwise = containsVar var rest



substitute :: Assign -> Expr -> Expr
substitute _ (Number n) = Number n
substitute assign (Variable var)
    | containsVar var assign = Number (value var assign)
    | otherwise = Variable var
substitute assign (Add expr1 expr2) = Add (substitute assign expr1) (substitute assign expr2)
substitute assign (Negate expr) = Negate (substitute assign expr)


normalize :: Expr -> Expr
normalize (Negate (Negate e)) = normalize e  -- Eliminate double negation
normalize (Negate (Number n)) = Number (-n)  -- Negate a number by negating its value
normalize (Negate (Add e1 e2)) = Add (normalize (Negate e1)) (normalize (Negate e2))  -- Move negation inside addition
normalize (Add e1 (Negate e2)) = Add (normalize e1) (Negate (normalize e2))  -- Move negation inside addition
normalize (Add e1 e2) = Add (normalize e1) (normalize e2)
normalize (Variable x) = Variable x  -- Variables remain as they are
normalize (Number n) = Number n  -- Numbers remain as they are
normalize (Negate e) = Negate (normalize e)  -- Move negation inside Negate


-----------
-- Tests --
-----------

testsAll = tests1 >> tests2 >> tests3 >> tests4 >> tests5

tests1 = checkAll "exercise 1" [
  (("x",exampleAssign),5),
  (("y",exampleAssign),12),
  (("y",exA),3),
  (("z",exB),9),
  (("x",exA),4),
  (("var",exA),7)
    ] (show . snd) (\ ((x,a),_) -> "value " ++ show x ++ " (" ++ show a ++ ")") 
      (uncurry value . fst)
      
tests2 = checkAll "exercise 2" [
  ((Empty, Number 5), 5),
  ((exampleAssign,exampleExpr),10),
  ((exA,exE),6),
  ((exA,exampleExpr),2)
    ] (show . snd) (\ ((a,e),_) -> "eval (" ++ show a ++ ") (" ++ show e ++ ")") 
      (uncurry eval . fst)
      
tests3 = checkAll "exercise 3" [
  (("x",exampleAssign),True),
  (("y",exampleAssign),True),
  (("z",exampleAssign),False),
  (("y",exA),True),
  (("variable",exB),False),
  (("var",exA),True)
    ] (show . snd) (\ ((x,a),_) -> "containsVar " ++ show x ++ " (" ++ show a ++ ")") 
      (uncurry containsVar . fst)
      
tests4 = checkAll "exercise 4" [
  ((Empty, Number 5), (Number 5)),
  ((exampleAssign,exampleExpr),Negate (Add (Number 5) (Negate (Add (Number 12) (Number 3))))),
  ((Empty,exampleExpr),exampleExpr),
  ((exampleAssign,exE),Negate (Add (Number (-3)) (Add (Number 5) (Negate (Variable "var"))))),
  ((Empty,exE),exE),
  ((exA,exampleExpr),Negate (Add (Number 4) (Negate (Add (Number 3) (Number 3)))))
    ] (show . snd) (\ ((a,e),_) -> "substitute (" ++ show a ++ ") (" ++ show e ++ ")") 
      (uncurry substitute . fst)
      
tests5 = checkAll "exercise 5" [
  (exampleExpr, Add (Negate (Variable "x")) (Add (Variable "y") (Number 3))),
  (exE, Add (Number 3) (Add (Negate (Variable "x")) (Variable "var"))),
  (Negate (Add (Number 5) (Number 8)), Add (Number (-5)) (Number (-8)))
    ] (show . snd) (\ (e,_) -> "normalize (" ++ show e ++ ")") 
      (normalize . fst)
      

-- internal construction of tests
aol = foldr (\ (x,y) -> Assign x y) Empty

exA = aol [("y",3),("x",4),("var",7)]
exB = aol [("z",9)]

exE = Negate (Add (Number (-3)) (Add (Variable "x") (Negate (Variable "var"))))

checkAll
  :: Show a =>
     String -> [b] -> (b -> String) -> (b -> String) -> (b -> a) -> IO ()
checkAll name xs e err c = do
    putStr ("*** " ++ name ++ ": ")
    let errors = filter (\x -> show (c x) /= e x) xs
    if null errors then putStrLn "OK"
    else do
        let x = head errors
        putStrLn ("ERROR; expexted '" ++ e x ++ "', but found '" ++ show (c x) 
          ++ "' for '" ++ err x ++ "'")