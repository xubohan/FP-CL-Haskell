-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 2
--
-- Week 2(23-27 Sep.)

import Data.Char
import Data.List
import Test.QuickCheck
import Control.Monad (guard)


-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [x  `div` 2 | x <- xs, even x ]


-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensReference xs


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x <- xs, x >= lo, x <= hi]


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = length [ x | x <- list, x > 0]


-- 4. multDigits

-- List-comprehension version

multDigits :: String -> Int
multDigits str = product [digitToInt x | x <- str, isDigit x]
  
  
  

countDigits :: String -> Int
countDigits str = length [digitToInt x | x <- str, isDigit x]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs <= 9 ^ (countDigits xs)


-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise s = toUpper (head s) : [toLower x | x <- tail s]


-- 6. title

lowercase :: String -> String
lowercase xs | length (xs) >= 4  = capitalise xs  
             | length (xs) < 4   = [toLower x | x <- xs]
             

-- List-comprehension version
title :: [String] -> [String]
title a = capitalise (head a): [lowercase x| x <- tail a ]


-- 7. signs

sign :: Int -> Char
sign i | i > 0 && i <= 9   = '+'
       | i == 0            = '0'
       | i >= -9 && i < 0  = '-'
       | otherwise         = error "it is out of range"

signs :: [Int] -> String
signs xs = [sign x| x <- xs]


-- 8. score

score :: Char -> Int
score x | isLower x && x /= 'a' && x /= 'e' &&
             x /= 'i' && x /= 'o' && x /= 'u'     = 1 
        | isUpper x && x /= 'A' && x /= 'E' &&
             x /= 'I' && x /= 'O' && x /= 'U'     = 2
        | x == 'a' || x == 'e' ||
             x == 'i' || x == 'o' || x == 'u'     = 2
        | x == 'A' || x == 'E' ||
             x == 'I' || x == 'O' || x == 'U'     = 3
        | not (isAlpha x)                         = 0

totalScore :: String -> Int
totalScore xs = product [score x | x <- xs, isAlpha x]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1

-- Tutorial Activity
-- 10. pennypincher
discount :: Int -> Int
discount x = fromIntegral(round(0.9*2))
-- List-comprehension version.
pennypincher :: [Int] -> Int
pennypincher prices = sum[discount x | x <- prices]

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs <= 19900

-- Optional Material

-- 11. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [ x | x <- words, (x !! pos) == letter, (length x) == len]


-- 12. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = [ value | (value, key) <- (zip [0 ..] str), key == goal]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
--prop_search str goal = and [(str !! a) == goal | (a , b) <- (zip (search str goal) [goal ..])]
prop_search str goal = length (search str goal) + length ([ value | (value, key) <- (zip [0 ..] str), key /= goal]) == length str

-- 13. contains

contains :: String -> String -> Bool
contains str substr = undefined

-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> String -> Bool
prop_contains str1 str2 = undefined

