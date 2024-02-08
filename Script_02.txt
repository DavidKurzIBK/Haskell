data Date = -- name of type
  DMY       -- name of constructor
    Int     -- day
    Int     -- month
    Integer -- year
  deriving Show

data Person = -- name of type
  Person      -- constructor name can be same as type name 
    String    -- first name
    String    -- last name
    Bool      -- married
    Date      -- birthday
  deriving Show

today :: Date
today = DMY 16 10 2023

myself, myself2, myself3 :: Person
myself = Person "Rene" "Thiemann" True today 
myself2 = Person "Rene" "Thiemann" True (DMY 16 10 2023)
myself3 = Person "Rene" "Thiemann" (5 > 3) (DMY 16 (length "0123456789") 2023)
{- 
not working: 
myself4 = Person ("Rene", "Thiemann", True, DMY (16, 10, 2023))
myself5 = Person "Rene" "Thiemann" True DMY 16 10 2023
-}

data Brand = Audi | BMW | Fiat | Opel deriving Show 
data Vehicle = 
    Car Brand Double -- horsepower
  | Bicycle
  | Truck Int -- number of wheels
  deriving Show

data Expr = 
    Number Integer
  | Variable String
  | Plus Expr Expr
  | Negate Expr
  deriving Show

data List = 
    Empty
  | Cons Integer List
  deriving Show

exampleExpression :: Expr
exampleExpression = Plus 
  (Negate 
     (Plus 
        (Number 5) 
        (Variable "x"))) 
  (Number 3)