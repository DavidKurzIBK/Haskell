import Prelude hiding (getLine)

getLine = do 
  c <- getChar
  if c == '\n'             -- branching
    then return ""  
    else do
      l <- getLine         -- recursion
      return $ c : l


data Expr = Const Double | Div Expr Expr
eval :: Expr -> Maybe Double
eval (Const c) = return c
eval (Div expr1 expr2) = do
  x1 <- eval expr1
  x2 <- eval expr2
  if x2 == 0 
    then Nothing
    else return (x1 / x2)
