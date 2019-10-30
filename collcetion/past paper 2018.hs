import Test.QuickCheck
import Data.Char

isVowel :: Char -> Bool
isVowel x = elem x "aeiouAEIOU"
m :: String -> Int
m x = 2 * length[a| a<-x, isVowel a] - length x  

n :: String -> Int
n "" = 0
n (x:xs)
        | isVowel x = 1 + n xs
        | otherwise = (-1) + n xs

prop_nm :: String -> Bool
prop_nm x = m x == n x

f :: String -> Bool
f [ ] = True
f (x:xs) = and [ isAlpha a /= isAlpha b | (a,b) <- zip (x:xs) xs]

g :: String -> Bool
g [ ] = True
g [a] = True
g (x:xs:y)
        | isAlpha x /= isAlpha xs = g (xs:y)
        | otherwise = False

prop_fg :: String -> Bool
prop_fg x = f x == g x

--------------------

vowelly :: Int -> Bool
vowelly 1 = True
vowelly 8 = True
vowelly 11 = True
vowelly 18 = True
vowelly x | x>=80 && x<=89 = True
          | otherwise = False

count :: [Int] -> Int
count x = sum [ 1 | a <- x, vowelly a]

countRec :: [Int] -> Int
countRec [ ] = 0
countRec (x:xs)
                | vowelly x = 1 + countRec xs
                | otherwise = countRec xs

prop_c :: [Int] -> Bool
prop_c x = count x == countRec x