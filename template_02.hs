

data Expr
  = LitInt Integer
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Show)


exampleExpression :: Expr
exampleExpression = Mul (Add (LitInt 3) (LitInt 1)) (LitInt 2)



data Expr1
  = LitBool Bool
  | LitInt1 Integer
  | Var String
  | Eq Expr1 Expr1
  | Neq Expr1 Expr1
  | Gt Expr1 Expr1
  | Gte Expr1 Expr1
  | Lt Expr1 Expr1
  | Lte Expr1 Expr1
  | And Expr1 Expr1
  | Or Expr1 Expr1
  deriving (Show)
  
  
exampleExpression1 :: Expr1
exampleExpression1 = And 
  (Or 
    (Gte (Var "z") (LitInt1 4))
    (Eq (Var "y") (Neq (LitInt1 7) (LitInt1 2)))
  )
  (Gt (Var "x") (LitInt1 3))


data Expr2
  = LitInt2 Integer
  | Var1 String
  | Call String [Expr2]
  | Mul1 Expr2 Expr2
  | Add1 Expr2 Expr2
  deriving (Show)


exampleExpression2 :: Expr2
exampleExpression2 = Mul1
  (Call "cube" [Add1 (LitInt2 4) (LitInt2 1)])
  (Mul1 (Mul1 (Var1 "height") (Var1 "width")) (Var1 "depth"))





data FridgeObject = QuantityObject String Int String  -- Name, Quantity, Expiration Date
  | VolumeObject String Float String  -- Name, Volume (in liters), Expiration Date
  deriving Show

applesA :: FridgeObject 
applesA = QuantityObject "Apples" 4 "October 31, 2023"

milkB :: FridgeObject
milkB = VolumeObject "Milk" 2.3 "August 4, 2023"

lemonsC :: FridgeObject
lemonsC = QuantityObject "Lemons" 6 "November 3, 2023"


data FridgeList = EmptyFridgeList | ConsFridgeObject FridgeObject FridgeList
  deriving Show

exampleFridgeList :: FridgeList
exampleFridgeList = ConsFridgeObject applesA (ConsFridgeObject applesA (ConsFridgeObject milkB EmptyFridgeList))



{-
The representation of an abstract syntax tree (AST) is not unique. 
Structure and design of an AST can vary depending on programming language, specific compiler or interpreter, and needs of the application. 
Different programming languages and tools may have different conventions for organizing AST nodes and may include additional information in the AST as needed.

There are common practices and conventions for representing AST's, like nodes representing various constructs like literals, operators (binary), variables, and function calls. 
These nodes have well-defined types to represent the various components of the language's grammar.
-}
