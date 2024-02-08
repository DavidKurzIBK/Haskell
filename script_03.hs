-- datatypes from previous lecture

data Date = -- name of type
  DMY       -- name of constructor
    Int     -- day
    Int     -- month
    Integer -- year
  deriving (Eq,Show)

data Person = -- name of type
  Person      -- constructor name can be same as type name 
    String    -- first name
    String    -- last name
    Bool      -- married
    Date      -- birthday
  deriving (Eq,Show)

data Expr = 
    Number Integer
  | Plus Expr Expr
  | Negate Expr
  deriving (Show,Eq)

data List = 
    Empty
  | Cons Integer List
  deriving (Show,Eq)

-- programs and datatypes in slides of week 3 


doubleNum  x = x + x     -- doubling a number
doubleExpr e = Plus e e  -- doubling an expression


today = DMY 23 10 2023
newborn fName lName = Person fName lName False today

withLastName lName (Person fName _ m b) = 
  Person fName lName m b

data Option = Some Integer | None deriving (Show,Eq)

ageYear (Person _ _ _ (DMY 23 10 y)) = Some (2023 - y)
ageYear _  = None

greeting p@(Person name _ _ _) = gHelper name (ageYear p)
gHelper n None = "Hello " ++ n
gHelper n (Some a) = "Hi " ++ n ++ ", you turned " ++ show a

conj True  b = b
conj False _ = False

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show,Eq)

weekend Sat = True
weekend Sun = True
weekend _   = False

first (Cons x xs) = x
first Empty       = error "first on empty list" 

len Empty = 0 
len (Cons x xs) = 1 + len xs -- len xs is recursive call

append Empty ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

eval (Number x)   = x
eval (Plus e1 e2) = eval e1 + eval e2
eval (Negate e)   = - eval e

numbers (Number x)   = Cons x Empty
numbers (Plus e1 e2) = append (numbers e1) (numbers e2)
numbers (Negate e)   = numbers e
