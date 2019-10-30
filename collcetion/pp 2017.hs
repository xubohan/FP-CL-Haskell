import Test.QuickCheck
import Data.Char

f :: [Int] -> Int
f x = sum [a*a | a <- x, a `mod` 3 == 0, a `mod` 5 /= 0]

g :: [Int] -> Int
g [ ] = 0
g (x:xs)
        | x `mod` 3 == 0
        && x `mod` 5 /= 0       = (x^2) + g xs
        | otherwise             = g xs

mst :: Int -> Int -> Bool
mst x y
      | x >= 0 &&
        y > 2*x          = True
      | x < 0 &&
        2*y > x          = True
      | otherwise       = False

ordered :: [Int] -> Bool
ordered [ ] = True
ordered (x:xs) = and [mst a b | (a,b) <- zip (x:xs) xs]

ordered' :: [Int] -> Bool
ordered' [ ] = True
ordered' [x] = True
ordered' (x:y:z) = mst x y && ordered' (y:z)

prop_t :: [Int] -> Bool
prop_t a = ordered a == ordered' a

-----------
c :: Char -> String -> String
c k [x] = [k]
c k [ ] = [ ]
c k str = [if even y then k else x| (x,y) <- zip str [0,1..]]

d :: Char -> String -> String
d k [x] = [k]
d k [ ] = [ ]
d k (x:y:z) = k : y : d k z

prop_cd :: Char -> String -> Bool
prop_cd k str = c k str == d k str

-----------
count :: String -> Int
count str = sum [1| x <- str, isUpper x || isDigit x]

countRec :: String -> Int
countRec [ ] = 0
countRec (x:xs)
              | isDigit x||
                isUpper x = 1 + countRec xs      
              | otherwise = countRec xs

prop_count :: String -> Bool
prop_count str = count str == countRec str

isNext :: Int -> Int -> Bool
isNext a b
         | even a &&
           a == 2*b     = True
         | odd a &&
           3*a +1 == b  = True        
         | otherwise = False

collatz :: [Int] -> Bool
collatz [ ] = True
collatz (x:xs) = and[ isNext a b| (a, b) <- zip (x:xs) xs] 

collatzRec :: [Int] -> Bool
collatzRec [ ] = True
collatzRec [x] = True
collatzRec (x:y:z) = isNext x y && collatzRec (y:z)

prop_check :: [Int] -> Bool
prop_check x = collatz x == collatzRec x

test_2 = quickCheck prop_check

---------
