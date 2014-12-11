--The prime factors of 13195 are 5, 7, 13 and 29.

--What is the largest prime factor of the number 600851475143 ?


is_prime :: Int -> Bool
is_prime n = null [x | x <- [2..(n-1)], n `mod` x == 0]

all_prime_factors a = [ x | x <- [1..a], a `mod` x == 0, is_prime x]

--all_prime_factors a = filter (is_prime)  (all_factors a)

biggest_prime_factor a = last (all_prime_factors a)