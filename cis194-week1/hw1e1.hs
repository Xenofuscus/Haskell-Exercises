toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x <= 0 = []
toDigitsRev n = [n `mod` 10] ++ toDigitsRev (n `div` 10)

-- Janky Reverse Helper Thing
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherHelper(reverse xs))

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper []     = []
doubleEveryOtherHelper (x:[]) = [x]
doubleEveryOtherHelper (x:y:rxs) = [x, y*2] ++ doubleEveryOtherHelper rxs

splitInt :: Integer -> [Integer]
splitInt x | x < 10 = [x]
splitInt x = splitInt(x `div` 10) ++ splitInt(x `mod` 10)

newIntList :: [Integer] -> [Integer]
newIntList [] = []
newIntList (x:xs)
    | x > 9 = splitInt(x) ++ newIntList(xs)
    | otherwise  = [x] ++ newIntList(xs)

sumDigits :: [Integer] -> Integer
sumDigits xs = sum(newIntList(xs))

validate :: Integer -> Integer
validate n = sumDigits(doubleEveryOther(toDigitsRev(n))) `mod` 10 == 0
