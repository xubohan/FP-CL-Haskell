import Data.Char
import Test.QuickCheck

f :: Char -> Bool
f 'g' = True
f 'j' = True
f 'p' = True
f 'q' = True
f 'y' = True
f  _  = False

g :: String -> Int
g str= sum [1 | x <- str, f x]

h :: String -> Int
h [ ] = 0
h (x:xs)
        | f x   = 1 + h xs
        | otherwise = h xs

prop_check :: String -> Bool
prop_check x = g x == h x


test_1 = quickCheck prop_check

----------------
c :: String -> String
c str = [if even a then toUpper b else b | (a,b) <- zip [0,1..] str]

d :: String -> String
d [ ] = [ ]
d [x] = [toUpper x]
d (x:y:z) = toUpper x : y : d z

prop_cd :: String -> Bool
prop_cd x = c x == d x

test_2 = quickCheck prop_cd

----------------
