{- Exercise 1 -}

-- TODO: add *most general* types of div1 - div4
-- construct values that are functions

-- Declare a function that takes two Fractional arguments and returns a Fractional result
div1 :: Fractional a => a -> a
div1 = (/ 2)  -- like a / 2. a is single argument and divides it by 2.
-- div( / 2) :: Fractional a => a -> a


div2 :: Fractional  a => a -> a
div2 = (2 /) -- like 2 / a .. 2 divided by single elem. a.
-- div(2 / ) :: Fractional a => a -> a


-- declares: (b -> c) -> (a -> b) -> (a -> c) which is 
div3 :: Fractional a => (a -> c) -> (a -> c) -- egal ob mit oder ohne klammer um den letzten Ausdruck, da es nicht eindeutig ist das nur ein Rückgabewert ausgegeben wird. 
div3 = (. (/ 2))

div4 :: Fractional a => (a -> c) -> a -> c
div4 = ((/ 2) .)

div5 :: Fractional a => (a -> b) -> (a -> b)
div5 f = f . div1

-- div3 f
-- = (. (/2)) f
-- = (\g -> g . (/2))
-- = f . (/2)!!!!!
-- = f . (\x -> x/2)
-- = \y -> f((\x -> x/2))
-- = \y -> f(y/2)

-- div5 f = f . div1
-- = f . (/2)!!!!! bewiesen


div6 :: Fractional a => a -> (a -> b) -> b
div6 x = \ f -> f (2 / x)

-- div7 :: Fractional a => ((a -> a) -> (a -> a), a) -> (a -> a)
div7 (f, x) = div3 f x

-- div8 :: Fractional a => ((a -> a), a) -> (a -> a)
div8 = \(f, x) -> f (x / 2) -- $ x

-- Question 1.1
{-
What do div1 and div2 do? Give an example that shows the difference between div1 and div2.

1. fractional a = type constraint for this subclass of number-type-class. type a must be instance of fractional type class.  
2. Fractional: provides operations for division and operations of fractions. Instances of Fractional include Float, Double, but not Rational.
3. most general type for div1 and div2 is function takes value of type a and returns value of the same type a.
4. a = 8: div(8/2) = 4
5. a = 8: div(2/8) = 0.25

-}

-- Question 1.2
{-
What do div3 and div4 do?  Give an example that shows the difference between div3 and div4.
(.) is composition operator. type is (b -> c) -> (a -> b) -> a -> c, takes two functions as arguments and returns a new function that composes them.
if a = 8: 
div3 performes div(smth is happening but (8/2) has priority) ?? 
div4 performes div ((smth happening to 8)/2) ?? 
-}

-- Question 1.3
{-
Which of the following pairs of functions are equal? Justify your answers.

(i) div3 and div5: 
div3 and div5 are not equal. 
div3 takes a function of type (a -> a) and returns another function of the same type, 
div5 takes a function of type (a -> b) and returns a function of the same type. 
Types of functions they operate on are different, so they are not equal.


(ii) div5 and flip div6: 
div5 and flip div6 are not equal. 
div5 takes a function and composes with div1, which is function of type (Fractional a => a -> a). 
flip div6 takes two arguments and applies them in reverse order. 
Types and behaviors of these functions are different. 


(iii) div7 and div8: 
div7 and div8 are not equal. 
div7 takes a tuple with a function of type (a -> a) -> (a -> a) and a value of type a and applies div3 to them,
div8 takes a tuple with a function of type (a -> a) and value of type a and applies different operation (f (x / 2)). 
Types and behaviors of these functions are different.

-}

{- Exercise 2 -}

type Name = String

type Age = Integer

type Salary = Double

data Employee = Employee Name Age Salary deriving (Show)

employees :: [Employee]
employees =
  [ Employee "Alice" 28 50000.0,
    Employee "Bob" 35 60000.0,
    Employee "Charlie" 42 75000.0,
    Employee "David" 30 55000.0,
    Employee "Eva" 25 48000.0
  ]

{- define 3 functions that operate on the Employee data type: 
all take Employee as its argument: 
1. returns Name of employee.
pattern (Employee n _ _) destructure Employee data type. matches the three fields of the Employee data type and binds them to the variables n, _, and _. 
underscores _ are placeholders for values not needed in this function.
result of function is the value of n, which is name of employee.

2. returns Salary of employee.
pattern (Employee _ _ s) destructure Employee data type. matches the three fields of the Employee data type and binds them to the placeholders _, _, and s.
result of function is the value of s, which is salary of employee.

3. returns Age of employee.
pattern (Employee _ a _) is used to destructure the Employee data type. matches the three fields of Employee data type and binds them to the placeholders _, a, and _.
result of function is value of a, which is age of employee.
-} 
getName :: Employee -> Name
getName (Employee n _ _) = n

getSalary :: Employee -> Salary
getSalary (Employee _ _ s) = s

getAge :: Employee -> Age
getAge (Employee _ a _) = a

--getEmployee :: Employee -> Employe
--getEmployee ( _ n a s) = e


{- Question 2.1
This function consrtucts a new employee by updating  all three fields (n, a, s) are func operating on name, age and salary of the employee data type with e argument itself: 
1. n (getName e): Applies function n to name of input employee (getName e). updates the name field of new Employee.
2. a (getAge e): -"- a to age of input employee (getAge e). -"- age field of new Employee.
3. s (getSalary e): -"- s to salary of input employee (getSalary e). -"- salary field of new Employee.
-} 
mapEmployee :: (Name -> Name) -> (Age -> Age) -> (Salary -> Salary) -> Employee -> Employee
mapEmployee n a s e = Employee (n (getName e)) (a (getAge e)) (s (getSalary e))

-- mapEmployee n a s (Employee name age salary) = Employee (n name) (a age) (s salary)  hier im pattern matching wird nicht auf die get-Funktionen zugegriffen!!




{- Question 2.2 function takes a list of Employee as input ([Employee]) and returns new list of Employee ([Employee]).
map: to apply a transformation to each element of the input list.
id: identity function, which means it does not modify the Name field of the employee; 
1. effectively advancing the age by one year.
2. increasing the salary by 20%.
-} 
nextYear :: [Employee] -> [Employee]
nextYear = map (mapEmployee id (+1) (*1.2))




{- Question 2.3
 -> function takes a list of Employee as input ([Employee]) and returns a list of tuples (Name, Salary).
 -> function composition (.) to combine two operations: filter and map.
1. filter applied first, input list of Employee instances based on condition defined by lambda function \ (Employee _ _ salary) -> salary < 60000. 
checks the salary of each employee is less than 60000.
2. map operation then applied to transform each filtered Employee into tuple (Name, Salary).
lambda function \ (Employee name _ salary) -> (name, salary) extracts the name and salary fields from each filtered Employee and creates a tuple
-}
lowIncomeEmployees :: [Employee] -> [(Name, Salary)]
lowIncomeEmployees = map (\(Employee name _ salary) -> (name, salary)) . filter (\(Employee _ _ salary) -> salary < 60000)



{- Question 2.4
Lambda functions often used in higher-order functions like map, filter, and foldr to create short, inline functions without need for a separate named function.
function that takes two arguments: comparison function lessOrEqual of type (a -> a -> Bool) that determines the order of elements -> returns a sorted list [a]
-}

qsortBy :: (a -> a -> Bool) -> [a] -> [a]
qsortBy _ [] = []  -- input list is empty, the result is an empty list (base case)

-- input list not empty, function recursively sorts two sublists:
-- smaller: Elements less than or equal to x.
-- larger: Elements greater than x.
-- result is obtained by concatenating sorted smaller, x, and sorted larger using the ++ operator.
qsortBy lessOrEqual (x:xs) = qsortBy lessOrEqual smaller ++ [x] ++ qsortBy lessOrEqual larger
    where													-- introduces local definitions that are used in the recursive case.
        smaller = filter (\y -> lessOrEqual y x) xs			-- Filters elements from xs are less than or equal to x, creating smaller sublist.
        larger = filter (\y -> not (lessOrEqual y x)) xs	-- -"- are greater than x, creating larger sublist.^^



{- Question 2.5
function takes a list of Employee instances as input ([Employee]) and returns a list of Name representing names of employees sorted by income.
1. input list of Employee sorted by qsortBy function by comparing employees based on their salaries in non-decreasing order (salary1 >= salary2).
2. result is passed to map function, which extracts names of employees from each sorted Employee instance, 
 lambda functions\ (Employee name _ _) are used to extract the name or salaries from each Employee instance.
-} 
employeesByIncome :: [Employee] -> [Name]
employeesByIncome = map (\(Employee name _ _) -> name) . qsortBy (\(Employee _ _ salary1) (Employee _ _ salary2) -> salary1 >= salary2)







-- TESTS --

checkEx2 = mapM_ putStrLn [test1, test2, test3, test4, test5]

test1 = check "nextYear" (show [Employee "Alice" 29 60000.0, Employee "Bob" 36 72000.0, Employee "Charlie" 43 90000.0, Employee "David" 31 66000.0, Employee "Eva" 26 57600.0]) (nextYear employees)

test2 = check "lowIncomeEmployees" (show [("Alice", 50000.0), ("David", 55000.0), ("Eva", 48000.0)]) (lowIncomeEmployees employees)

test3 = check "qsortBy1" (show [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]) (qsortBy (>=) [1 .. 10])

test4 = check "qsortBy2" (show ["hi", "and", "world", "Innsbruck"]) (qsortBy (\xs ys -> length xs <= length ys) ["hi", "world", "and", "Innsbruck"])

test5 = check "employeesByIncome" (show ["Charlie", "Bob", "David", "Alice", "Eva"]) (employeesByIncome employees)

check :: Show a => [Char] -> String -> a -> [Char]
check name e c =
  "*** "
    ++ name
    ++ ": "
    ++ ( if show c == e
           then "OK"
           else "ERROR; expected '" ++ e ++ "', but found '" ++ show c ++ "'"
       )