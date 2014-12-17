--A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

--a2 + b2 = c2
--For example, 32 + 42 = 9 + 16 = 25 = 52.

--There exists exactly one Pythagorean triplet for which a + b + c = 1000.
--Find the product abc.


--find numbers x +y+z =1000


-- SLOW BF -- 1 min

numbers :: Int
numbers = [x*y*z | x <- [1..1000], y <- (list2 x), z <- (list2 x), triplet x y z]
		where
			list2 x = filter (\el -> el >= x) [1..1000]


triplet :: Int -> Int -> Int -> Bool
triplet a b c = sum1000 && pythCondition
			where
				sum1000 = a + b + c == 1000
				pythCondition = (a^2) + (b^2) == (c^2) 


