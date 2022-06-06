module Exercises where
import Data.List
import System.IO

sumSqr = sum [x^2 | x <- [1..100]]

grid n m = [(x,y) | x <- [0..m], y <- [0..n]]

sqrGrid n = [(x,y) | (x, y) <- grid n n, x /= y]

replicate n x = [ x | _ <-[1..n]]

pyths n = [(x,y,z) | x <- [1..n],
                     y <- [1..n],
                     z <- [1..n],
                     x^2 + y^2 == z^2]

factors n = [x | x <- [1..n-1], n `mod` x == 0]
perfects n = [x | x <- [1..n], sum (factors x) == x]
-- another way
factors' n = [x | x <- [1..n], n `mod` x == 0]
isPerfect n = sum (init (factors' n)) == n -- init will drop last element
perfect n = [x | x <- [1..n], isPerfect x]

com1 = [x | x <- [1,2] ]
com2 = [y | y <- [3,4] ]
coms = concat [com1, com2]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [xs !! i * ys !! i | i <- [0.. (length xs - 1)]]
-- another way
scalarproduct' xs ys = sum [x*y | (x,y) <- zip xs ys]

fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n - 1)

-- (^) :: Int -> Int -> Int
-- m ^ 0 = 1
-- m ^ n = m * (m ^ (n-1))

euclid :: Int -> Int -> Int
euclid m n | m == n = m
           | m < n = euclid m (n - m)
           | m > n = euclid (m - n) n

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == False = False
           | otherwise = and xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ (concat' xs)

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x:replicate' (n-1) x

(!!@) :: [a] -> Int -> a
(!!@) (x:xs) 0 = x
(!!@) (x:xs) n = (!!@) xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' e (x:xs) | x == e = True
               | otherwise = elem' e xs

merge' :: Ord a => [a] -> [a] -> [a]
merge' [] [] = []
merge' [] xs = xs
merge' ys [] = ys
merge' (x:xs) (y:ys) | x <= y = x:merge' xs (y:ys)
                     | otherwise = y: merge' ys (x:xs)
