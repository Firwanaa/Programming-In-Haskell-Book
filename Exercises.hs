module Exercises where
import Data.List
import System.IO

-- 1
-- [Char]
-- (Char, Char, Char)
-- [(Bool, Char)]
-- ([Bool], [Char])
-- [a]
-- 2
-- [True, False]
-- [[1,2],[2,4]]
-- f a b c = a + b + c
-- f x = (x,x)
-- f x = f x
-- 3
-- second :: [a] -> a
-- swap :: (a,b) -> (b,a)
-- pair :: a -> b -> (a,b)
-- double :: Num a => a -> a
-- palindrome :: Eq a => [a] -> Bool -- cheated
-- twice :: (a->b) -> a -> b

halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

third1 xs = head (tail (tail xs))
third2 xs = xs !! 2
third3 (_:_:x:_) = x

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
concat' (xs:xss) = xs ++ (concat' xss)

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x:replicate' (n-1) x

(!!@) :: [a] -> Int -> a
(!!@) (x:_) 0 = x
(!!@) (_:xs) n = (!!@) xs (n-1)

insert' :: Int -> [Int] -> [Int]
insert' x [] = [x]
insert' n (x:xs) | n <= x = n:x:xs
                 | otherwise = x:insert' n xs

-- Insertion sort
isort' :: [Int] -> [Int]
isort' [] = []
isort' (x:xs) = insert' x (isort' xs)

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' e (x:xs) | x == e = True
               | otherwise = elem' e xs

merge' :: Ord a => [a] -> [a] -> [a]
merge' [] [] = []
merge' [] xs = xs
merge' ys [] = ys
merge' (x:xs) (y:ys) | x < y = x:merge' xs (y:ys)
                     | otherwise = y: merge' ys (x:xs)

halve' :: [a] -> ([a], [a])
halve' xs = (take n xs, drop n xs)
           where n = length xs `div` 2
-- merge sort
msort' :: Ord a => [a] -> [a]
msort' [] = []
msort' [x] = [x]
msort' xs = merge' (msort' ys) (msort' zs)
            where (ys, zs) = halve' xs

f xs = map f (filter p xs)

all :: (a -> Bool) -> [Bool] -> Bool
all p = and . map p

any p = or . map p

takeWhile [] = []
takeWhile p (x:xs) | p x = x:takeWhile p xs
                   | otherwise = []

dropWhile [] = []
dropWhile p (x:xs) | p x = dropWhile xs
                   | otherwise = x:xs
map' f [] []
map' f (x:xs)= foldr (\y ys -> (f y):ys) [] xs

map2 f = foldr (\x xs -> f x:xs) []

filter' p = foldr (\x xs -> if p x then x:xs else xs)

dec2int' :: [Int] -> Int
dec2int' = foldl (\x y -> 10*x +y) 0

-- curry
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x , y)

-- uncurry
uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x,y) -> f x y

-- unfold
unfold' p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)
int2bin' = unfold' (== 0) (`mod` 2) (`div` 2)
chop8' = unfold' (== []) (take 8) (drop 8)
map' f = unfold' null (f.head) tail
iterate f = unfold'

iterate f = unfold (const False) id f -- totally cheating.
-- will come back to the rest later

-- Exercise
-- 1 - Using recursion and function add, define a function that
--     multiplies two natural numbers.
mult :: Nat -> Nat -> Nat
mult Zero m = Zero
mult (Succ n) m = add (mult n m) m

-- test on ghci
-- res = mult (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- print $ nat2int res
-- 6

-- 2 - Define a suitable function folde for expressions and give
--     a few examples of its use.
data Expr' = Val' Int | Add' Expr' Expr'
folde :: (Int -> a) -> (a -> a -> a) -> Expr' a
folde f g (Val' n) = n
folde f g (Add ex1 ex2) = g (folde f g ex1) (fold f g ex2)

-- 3 - Define typ "Tree a" of binary trees built from "Leaf"
--     values of type a using a Node constructor that takes two binary trees
--      as parameters.
data Tree' a = Leaf' a
             | Node' (Tree' a) (Tree' a)

-- different types of trees
data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a) -- data in leaves only
data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a) -- data in Nodes only
data Tree3 a b = Leaf3 a | Node3 (Tree3 a b) b (Tree3 a b) -- different data in both leaves and nodes
data Tree4 a = Node4 a [Tree4 a] -- List of subtrees, empty List willserve as leafe

-- will come back to the rest of the questions later

choices :: [a] -> [[a]]
choices xs = [ys | yss <- subs xs, ys <- perms yss]

dropfirst :: [Eq] => a -> [a] -> [a]
dropfirst x [] = []
dropfirst x (y:yss) | x == y = ys
                    | otherwise = drpfirst x ys

isChoice :: Eq => [a] -> [a]
isChoice [] _ = True
isChoice (x:xs) [] = False
isChoice (x:Xs) ys = elem x ys && isChoice xs (dropfirst x ys)

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <=y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y/= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0
valid Exp x y = y  >= 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y
