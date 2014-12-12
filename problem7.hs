

--By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

--What is the 10 001st prime number?

--Eratosthenes' sieve --> SLOW ±1-2min
primes :: [Int]
primes = sieve [2..]
	where sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

prime :: Int -> Int
prime n = primes !! (n-1) 

--is_prime :: Int -> Bool
--is_prime x = (null . filter (\y -> x `mod` y == 0) ) [2..(x-1)] 

---- prime n = prime (n-1) actually
--prime :: Int -> Int
--prime n = [x | x <- [2..], is_prime x] !! (n-1) 