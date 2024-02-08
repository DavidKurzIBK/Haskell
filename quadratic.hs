
square :: Double -> Double
square x = x^2 

discr a b c = sqrt(b^2 - 4 *a*c)


quadratic a b c = ( - b + discr a b c / (2 * a), - b - discr a b c / (2 * a))