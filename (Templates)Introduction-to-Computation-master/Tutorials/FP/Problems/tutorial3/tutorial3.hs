-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 3
--
-- Week 3(30-04 Oct.)
module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck

import Data.Function
import Data.Maybe


-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec = undefined

-- halveEvens :: [Int] -> [Int]
-- halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = undefined


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec = undefined

-- inRange :: Int -> Int -> [Int] -> [Int]
-- inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = undefined


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec = undefined

-- countPositives :: [Int] -> Int
-- countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives l = undefined


-- 4.

multDigitsRec :: String -> Int
multDigitsRec = undefined

-- multDigits :: String -> Int
-- multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = undefined


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================


-- 5.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch xs = undefined

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec = undefined

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = undefined


-- 6.

encipher :: Int -> Char -> Char
encipher k ch = undefined


-- 7.

normalize :: String -> String
normalize = undefined 


encipherStr :: Int -> String -> String
encipherStr k str = undefined


-- 8.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey = undefined

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec = undefined

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey = undefined


-- 9.

decipher :: Int -> Char -> Char
decipher = undefined

decipherStr :: Int -> String -> String
decipherStr = undefined

-- Optional Material
-- =================


-- 10.

contains :: String -> String -> Bool
contains = undefined


-- 11.

candidates :: String -> [(Int, String)]
candidates = undefined


-- 12.

splitEachFive :: String -> [String]
splitEachFive = undefined

prop_transpose :: String -> Bool
prop_transpose = undefined


-- 13.
encrypt :: Int -> String -> String
encrypt = undefined


-- 14.
decrypt :: Int -> String -> String
decrypt = undefined
