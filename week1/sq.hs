import Test.QuickCheck

square :: Integer -> Integer
square x = x * x

pyth :: Integer -> Integer -> Integer
pyth a b = square a + square b

prop_square :: Integer -> Bool
prop_square x = square x >= 0

prop_squares :: Integer -> Integer -> Bool
prop_squares x y =
  square (x+y) == square x + 2 * x * y + square y

prop_pyth :: Integer -> Integer -> Bool
prop_pyth x y =
   square (x+y) == pyth x y + 2 * x * y

nums :: [Int]
nums = [1,2,3]

chars :: [Char]
chars = ['I' , 'n' , 'f' , 'l']

str :: String
str = "Inf1"

numss :: [[Int]]
numss = [[1], [2,4,2], [ ], [3,5]]

count :: [Int]
count = [1..10]

squares :: [Int] -> [Int]
squares xs = [ x * x | x <-xs]

odds :: [Int] -> [Int]
odds xs = [ x | x <- xs, odd x]

sumSqOdd :: [Int] -> Int
sumSqOdd xs = sum [ x * x | x <- xs, odd x]

prop_sumSqOdd :: [Int] -> Bool
prop_sumSqOdd xs = sum (squares (odds xs)) == sumSqOdd xs

factorial :: Integer -> Integer
factorial a
  | a == 0      = 1
  | a > 0       = factorial (a - 1) * a

oddsRec :: [Int] -> [Int]
oddsRec []       = []
oddsRec (x:xs) | odd x  = x : oddsRec xs
               | otherwise = oddsRec xs

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."

