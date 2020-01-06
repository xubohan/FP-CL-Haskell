-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 3
--
-- Week 3(30-04 Oct.)
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck

import Data.Function
import Data.Maybe


-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | x `mod` 2 == 0 = x `div` 2 : halveEvensRec xs
                     | otherwise = halveEvensRec xs

halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensRec xs == halveEvens xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec lo hi (x:xs) | lo <= x && x <= hi = x : inRangeRec lo hi xs
                        | otherwise = inRangeRec lo hi xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRangeRec lo hi xs == inRange lo hi xs


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) = countPositivesRec xs + if x > 0 then 1 else 0

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives l = countPositives l == countPositivesRec l


-- 4.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x = digitToInt x * multDigitsRec xs
                     | otherwise = multDigitsRec xs

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigitsRec xs == multDigits xs


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
lookUp ch xs = head ([ y | (x,y) <- xs, x == ch] ++ [ch])

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ch
lookUpRec ch ((key,val):restKey)
    | key == ch = val
    | otherwise = lookUpRec ch restKey

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c key = lookUp c key == lookUpRec c key


-- 6.

encipher :: Int -> Char -> Char
encipher k ch = lookUp ch (makeKey k)


-- 7.

normalize :: String -> String
normalize [] = []
normalize (ch:str)
    | isAlpha ch = toUpper ch : normalize str
    | isDigit ch = ch : normalize str
    | otherwise  = normalize str


encipherStr :: Int -> String -> String
encipherStr k str = [encipher k ch | ch <- normalize str]


-- 8.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey key = [(b, a) | (a, b) <- key]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((k,v):keys) = (v,k) : reverseKeyRec keys

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey key = reverseKey key == reverseKeyRec key


-- 9.

decipher :: Int -> Char -> Char
decipher k ch = lookUp ch (reverseKey (makeKey k))

decipherStr :: Int -> String -> String
decipherStr k str = [decipher k ch | ch <- str, isUpper ch || isDigit ch || ch == ' ']


-- Optional Material
-- =================


-- 10.

contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains str substr = 
    isPrefixOf substr str || contains (tail str) substr


-- 11.

candidates :: String -> [(Int, String)]
candidates str = [(i, decipherStr i str) | i <- [0..25], candidate (decipherStr i str)]
    where candidate str = str `contains` "AND" || str `contains` "THE"


-- 12.

splitEachFive :: String -> [String]
splitEachFive xs | length xs > 5 = take 5 xs : splitEachFive (drop 5 xs)
                 | otherwise     = [ fillToFive xs ]

fillToFive :: String -> String
fillToFive xs = xs ++ replicate (5 - length xs) 'X'

-- An alternative solution demonstrating 'repeat'
fillToFive' :: String -> String
fillToFive' xs = take 5 (xs ++ repeat 'X')


prop_transpose :: String -> Bool
prop_transpose xs = ys == transpose (transpose ys)
    where
      ys = splitEachFive xs

-- The following example shows why 'transpose' is not
--  invertible in general. The transpose function
--  takes the 'columns' of a list of lists, and makes
--  them the 'rows' of a new list of lists. 
--
-- [[o n e],           [[o t t f f],       [[o n e e e],
--  [t w o],            [n w h o i],        [t w o r],  
--  [t h r e e],   -->  [e o r u v],   -->  [t h r e],  
--  [f o u r],          [e r e],            [f o u], 
--  [f i v e]   ]       [e],        ]       [f i v]     ]   


-- 13.

encrypt :: Int -> String -> String
encrypt n str = concat (transpose (splitEachFive (encipherStr n str)))
-- 16.
splitFiveWays :: String -> [String]
splitFiveWays xs | n `mod` 5 == 0 = splitEach (n `div` 5) xs
                 | otherwise      = error "splitFiveWays: not a multiple of 5"
                 where n = length xs

splitEach :: Int -> String -> [String]
splitEach _ [] = []
splitEach n xs = take n xs : splitEach n (drop n xs)

-- 14.
decrypt :: Int -> String -> String
decrypt n str = concat (transpose (splitFiveWays (decipherStr n str)))
