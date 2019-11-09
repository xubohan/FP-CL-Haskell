-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 2
--
-- Week 2(23-27 Sep.)
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck
import Control.Monad (guard)


-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]


-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensReference xs == halveEvens xs


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

-- A list-comprehension version without library functions is not
-- possible, because list-comprehension examines items in a list
-- individually; it cannot use functions (like "+") between them.


-- 4. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

countDigits :: String -> Int
countDigits str = length [ 42 | ch <- str, isDigit ch ]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs <= 9 ^ countDigits xs


-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = toUpper x : [toLower x | x <- xs]


-- 6. title

-- auxiliary functions used by both
capitaliseLong :: String -> String
capitaliseLong word | length word >= 4 = capitalise word
                    | otherwise        = lowercase word

lowercase :: String -> String
lowercase xs = [toLower x | x <- xs]

-- List-comprehension version
title :: [String] -> [String]
title [] = []
title (w:words) = capitalise w : [capitaliseLong w | w <- words]

-- -- Recursive version
-- titleRec :: [String] -> [String]
-- titleRec [] = []
-- titleRec (w:words) = capitaliseRec w : titleAuxRec words
--   where titleAuxRec [] = []
--         titleAuxRec (w:words) = capitaliseLongRec w : titleAuxRec words

-- capitaliseLongRec :: String -> String
-- capitaliseLongRec word | length word >= 4 = capitaliseRec word
--                        | otherwise        = lowerRec word


-- -- mutual test
-- prop_title :: [String] -> Bool
-- prop_title xs = title xs == titleRec xs



-- 7. signs

sign x | 0 < x && x <= 9     = '+'
       | 0 == x              = '0'
       | 0 > x && x >= (-9)  = '-'
       | otherwise           = error ("sign: argument "++show x++" is out of range")

signs :: [Int] -> String
signs xs = [sign x | x <- xs, (-9) <= x, x <= 9]

-- 8. score

score :: Char -> Int
score x  =  s (isAlpha x) + s (isUpper x) + s (elem (toLower x) "aeiou")
  where
  s t  =  if t then 1 else 0 

totalScore :: String -> Int
totalScore xs = product [ score x | x <- xs, score x > 0 ]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1


-- Tutorial Activity
-- 10. pennypincher

-- Helper function
discount :: Int -> Int
discount price = round ((fromIntegral price) * 0.9)

-- List-comprehension version.
pennypincher :: [Int] -> Int
pennypincher prices = sum [discount price | price <- prices, discount price <= 19900]

-- -- Recursive version
-- pennypincherRec :: [Int] -> Int
-- pennypincherRec [] = 0
-- pennypincherRec (price:prices)
--     | discount price <= 19900 = discount price + pennypincherRec prices
--     | otherwise               = pennypincherRec prices


-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs <= sum [x | x <- xs, x > 0]

-- Optional Material

-- 11. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words =
    [w | w <- words,  
         length w == len,
         0 <= pos,
         pos < len, 
         w !! pos == letter]


-- 12. search

-- List-comprehension version

-- This solution demonstrates the use of an infinite list [0..] of
-- indices. The "zip" function only zips pairs of elements together
-- as long as it has two elements to pair;  when it gets to the end
-- of one list, it stops.
search :: String -> Char -> [Int]
search str goal = [i | (ch, i) <- zip str [0..], ch == goal]

-- The length of the search result should be the same as the number of goal characters in the string.
prop_search :: String -> Char -> Bool
prop_search str goal = length [ 42 | c <- str, c == goal] == length (search str goal)


-- 13. contains

-- Get a list of all "tails" of a list
suffixes :: String -> [String]
suffixes xs = [drop i xs | i <- [0..length xs]]

-- Check these against the prefix and verify that there are
-- at least some that match
contains :: String -> String -> Bool
contains str substr = [] /= [ True | s <- suffixes str, isPrefixOf substr s ]

-- Longer strings are never contained in shorter strings
prop_contains :: String -> String -> Bool
prop_contains str1 str2 | length str1 < length str2 = not (contains str1 str2)
prop_contains str1 str2 | length str1 > length str2 = not (contains str2 str1)
prop_contains str1 str2 = not (contains str1 (str2 ++ "abba"))
