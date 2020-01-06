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
halveEvens xs = undefined


-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = undefined


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = undefined


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = undefined


-- 4. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = undefined

countDigits :: String -> Int
countDigits str = undefined

prop_multDigits :: String -> Bool
prop_multDigits xs = undefined


-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise s = undefined


-- 6. title

lowercase :: String -> String
lowercase xs = undefined

-- List-comprehension version
title :: [String] -> [String]
title _ = undefined


-- 7. signs

sign :: Int -> Char
sign i = undefined

signs :: [Int] -> String
signs xs = undefined


-- 8. score

score :: Char -> Int
score x  = undefined

totalScore :: String -> Int
totalScore xs = undefined

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = undefined

-- Tutorial Activity
-- 10. pennypincher

-- List-comprehension version.
pennypincher :: [Int] -> Int
pennypincher prices = undefined

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = undefined

-- Optional Material

-- 11. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = undefined


-- 12. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = undefined

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = undefined


-- 13. contains

contains :: String -> String -> Bool
contains str substr = undefined

-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> String -> Bool
prop_contains str1 str2 = undefined
