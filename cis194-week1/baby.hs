x :: Int
x = 3

y :: Int
y = y + 1

biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

foo :: Integer -> Integer
foo 0 = 16
foo 1
    | "Haskell" > "C++" = 3
    | otherwise         = 4
foo n
    | n < 0             = 0
    | n `mod` 17 == 2   = -43
    | otherwise         = n + 3

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

f :: Int -> Int -> Int -> Int
f x y z = x + y + z
ex17 = f 3 17 8

nums, range, range2 :: [Integer]
nums = [1,2,3,19]
range = [1..100]
range2 = [2,4..100]

intListLength :: [Integer] -> Integer
intListLength []    = 0
intListLength (x:xs) = 1 + intListLength xs

tail' (_:xs)     = xs