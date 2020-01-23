-- Contains basic functions for a right angled triangle to demonstrate maths lib usage

pythag :: Floating a => a -> a -> a
pythag x y = sqrt (x^2 + y^2)

perimeter :: Floating a => a -> a -> a
perimeter x y = x + y + pythag x y

area :: Floating a => a -> a -> a
area x y = 0.5 * x * y