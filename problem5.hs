

--2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

--What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?


--beter met LCM builtin; least common denominator

lcm' :: [Int] -> Int
lcm' list = foldl1 lcm list

solution :: Int -> Int -> Int
solution n m = lcm' [n..m]

-----

smallest_multiple :: Int -> Int -> Int -> Int
smallest_multiple x n m
    | all_zeros mapping == True = x
    | otherwise = smallest_multiple next n m
    where
        mapping = map (x `mod`) [n..m]
        next = foldl1 (+) [n..m]

all_zeros :: [Int] -> Bool
all_zeros [0] = True
all_zeros l@(x:xs)
    | x == 0 = all_zeros xs
    | otherwise = False

no_remainder :: Int -> Int -> Bool
no_remainder = (\a b -> a `mod` b == 0)