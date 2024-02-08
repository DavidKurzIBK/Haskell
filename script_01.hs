-- This script is stored in file script_01.hs
average x y = (x + y) / 2

{- 
the following function takes a temperature in
degree Fahrenheit and coverts it into Celsius 
-}

fahrenheitToCelsius f = (f - 32) * 5 / 9

-- combining functions is also possible
averageTempInCelsius f1 f2 = average (fahrenheitToCelsius f1) (fahrenheitToCelsius f2)