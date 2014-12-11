

--If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
--The sum of these multiples is 23.

--Find the sum of all the multiples of 3 or 5 below 1000.


better = sum [n | n <- [1..1000-1], n `mod` 5 == 0 || n `mod` 3 == 0]

sum_multiples :: Int -> Int -> Int -> Int
sum_multiples x y n  = 	foldl1 (+) (join' [x, x+x..(n-1)] [y, y+y..(n-1)])

join' :: [Int] -> [Int] -> [Int]
join' l1 l2
	| l1 == [] = l2
	| l2 == [] = l1
	| e1 > e2 = e2 : join' l1 (tail l2)
	| e1 < e2 = e1 : join' (tail l1) l2
	| otherwise = e1 : join' (tail l1) (tail l2)
	where 
		e1 = head l1
		e2 = head l2


