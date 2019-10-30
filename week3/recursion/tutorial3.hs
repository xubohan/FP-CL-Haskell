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
halveEvensRec [] = []
halveEvensRec (x:xs) | x `mod` 2 == 0 = x `div` 2 : halveEvensRec xs
                     | otherwise      = halveEvensRec xs
                     

halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensRec xs == halveEvens xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
--inRangeRec _ _ [ ] = [ ]
--inRangeRec lo hi (x:xs)  | lo > hi          = [ ]
--                         | lo <= hi &&
--                           x >= lo          = x : inRangeRec (lo+1) hi xs
--                         | otherwise        = inRangeRec lo hi xs
inRangeRec a b [] = []
inRangeRec a b (x:xs)
           | a <= x && x <=b = x: inRangeRec a b xs
           | otherwise       = inRangeRec a b xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRangeRec lo hi xs == inRange lo hi xs


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec  [] = 0
countPositivesRec (x:xs) | x > 0        = 1 +  countPositivesRec xs
                         | otherwise    = countPositivesRec xs

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives l = countPositivesRec l == countPositives l


-- 4.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x        = (digitToInt x) * multDigitsRec xs
                     | otherwise        = multDigitsRec xs

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
lookUp ch xs = head [ b | (a ,b) <- xs, a == ch]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ' ' 
lookUpRec ch ((a,b):xs) | a == ch     = b
                        | isDigit ch  = ch
                        | otherwise   = lookUpRec ch xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUp c k == lookUpRec c k


-- 6.

encipher :: Int -> Char -> Char
encipher k ch = lookUpRec ch (makeKey k)
        


-- 7.

normalize :: String -> String
normalize [] = []
normalize (x:xs) | isAlpha x ||
                   isDigit x    = toUpper x : normalize xs
                 | otherwise    = normalize xs


encipherStr :: Int -> String -> String
encipherStr k [ ] = [ ]
encipherStr k (x:xs) = encipher k (toUpper x) : encipherStr k (normalize xs)
--encipherStr k str
--                 if (str == " ") then
--                    [ ]
--                 else
--                    let
--                      x = head (normalize str)
--                      y = tail (normalize str)
--                    in
--                      encipher k x : encipherStr k str
                     


-- 8.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey str= [ (b, a) | (a,b) <- str]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [ ] = [ ]
reverseKeyRec ((a,b):xs) = (b,a):reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey str= reverseKeyRec str == reverseKey str


-- 9.

decipher :: Int -> Char -> Char
decipher num x= lookUpRec x (reverseKeyRec (makeKey num))

decipherStr :: Int -> String -> String
decipherStr num [ ] = [ ]
decipherStr num (x:xs) = decipher num x : decipherStr num xs

-- Optional Material
-- =================


-- 10.

contains :: String -> String -> Bool
contains a b =  or( check a b )
  where
  check :: String -> String ->[Bool]
  check [ ] str = [False]
  check  (y:ys) str | y == (head str) = isPrefixOf (tail str) ys :  check ys str
                    | otherwise       = check ys str
-- 11.

candidates :: String -> [(Int, String)]
--recursion
--candidates [ ] = [ ]
--candidates s | check s a= 
--             | otherwise = candidates s
 
--List comprehension
candidates str = [(a, b) | (a, b)<-zip [1..26]  [decipherStr a str| a <- [1..26]], contains b "THE" || contains b "AND"]



-- 12.

splitEachFive :: String -> [String]
splitEachFive [ ] = [ ]
splitEachFive str = take 5 (addX str) : splitEachFive (drop 5 str)
  where
  addX :: String -> String
  addX str | (length str) `mod` 5 /= 0 = str ++ (replicate (5 -((length str) `mod` 5)) 'X')
           | otherwise = str

prop_transpose :: String -> Bool
prop_transpose = undefined


-- 13.
encrypt :: Int -> String -> String
encrypt num x = concat (transpose (splitEachFive (encipherStr num x)))


-- 14.
decrypt :: Int -> String -> String
decrypt = undefined
