

--A palindromic number reads the same both ways. 
--The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
--Find the largest palindrome made from the product of two 3-digit numbers.

reverseInt :: Int -> Int
reverseInt = read . reverse . show


is_palindrome :: Int -> Bool
is_palindrome original
	| reversed == original = True
	| otherwise = False
	where reversed = reverseInt original

maxi = 999

math :: Int -> Int -> [Int]
math 0 0 = []
math 0 m = math maxi (m-1)
math n m = (n*m) : (math (n-1) m)

f :: Int
f = maximum (filter (is_palindrome) (math 999 999)) 
----palindrome :: Int -> Int -> Int
--palindrome 0 m = palindrome maxi (m-1)
--palindrome n m
--	| is_palindrome result = result
--	| otherwise = palindrome (n-1) m
--	where result = n*m
