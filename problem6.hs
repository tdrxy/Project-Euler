

--The sum of the squares of the first ten natural numbers is,
--12 + 22 + ... + 102 = 385

--The square of the sum of the first ten natural numbers is,
--(1 + 2 + ... + 10)^2 = 55^2 = 3025

--Hence the difference between the sum of the squares of the first ten natural numbers and the square 
--of the sum is 3025 − 385 = 2640.

--Find the difference between the sum of the squares of the first one hundred natural numbers and the square 
--of the sum.

sum_squares :: Int -> Integer
sum_squares n = toInteger . foldl1 (+) . map (\x -> x*x) $ [1..n]
square_sum :: Int -> Integer
square_sum n = toInteger $ result * result 
				where result = foldl1 (+) [1..n] 

difference :: Int -> Integer
difference n = square_sum n - sum_squares n