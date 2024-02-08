-- replace "undefined" by your definition

import Prelude

volume :: Double -> Double
volume r = (4/3) * pi * r^3


-- Constants
litersPerCubicDecimeter :: Double
litersPerCubicDecimeter = 1.0

costPerLiter :: Double
costPerLiter = 50.99 / 400.0

-- Function to calculate the volume of a sphere
sphereVolume :: Double -> Double
sphereVolume r = (4/3) * pi * (r / 10.0) ^ 3

-- Function to calculate the cost of filling a balloon
heliumCosts :: Double -> Double
heliumCosts r = costPerLiter * (litersPerCubicDecimeter * sphereVolume r)

-- Inverse function to calculate balloon radius
balloonRadius :: Double -> Double
balloonRadius m = ((m / costPerLiter) ** (1/3)) * 1000.0



-- the following tests can be used by you, once you have implemented your functions,
-- for testing, just invoke testV, testHC1, testHC2, testBR1, testBR2 in ghci

testV = "test volume: expected ~ 1436.755, computed: " ++ show (volume 7 :: Double)
testHC1 = "test heliumCosts 1: expected ~ 4.27, computed: " ++ show (heliumCosts 20 :: Double)
testHC2 = "test heliumCosts 2: expected ~ 34.17, computed: " ++ show (heliumCosts 40 :: Double)
testBR1 = "test ballonRadius 1: expected ~ 21.08, computed: " ++ show (balloonRadius 5 :: Double)
testBR2 = "test ballonRadius 2: expected ~ 26.56, computed: " ++ show (balloonRadius 10 :: Double)
