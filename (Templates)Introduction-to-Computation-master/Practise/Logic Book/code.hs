-- import Data.Set
import Data.List
-- it should be list ahead of any funtions  

-- Types in Haskell 
x :: Int 
x = 25 - 3
-- int ghci we enter 
-- let x :: Int; x= 25 - 3

y :: Float 
y = 3.14

s :: [Char]
s = "hello"

s1 :: String 
s1 = "hello"

-- :t True 

f :: Int -> Int 
f x = x ^ 2

f' :: Int -> Int -> Int
f' x y = f x + f y
-- use "'" but not "`"
-- the name of the function declaration should start with a lowercase letter 


-- three ways to specify sets
-- 1 List 
data City = Guangzhou | Stockholm | Edingburgh | Boston 
-- let haskell know 
cities :: [City]
cities = [Guangzhou, Stockholm, Edingburgh,Boston]
-- On the contrast, the name of the set defined in data, should start with a Uppercase letter

-- 2 Select
div2and3 :: [Int]
div2and3 = [x | x <- [1..],x `mod` 2 == 0 , x `mod` 3 == 0 ]

-- 3 Generate 
evens :: [Int]
evens = 0 : [x + 2 | x <- evens]


-- ghci > [1,1,1,2,3,3,4,5] == [3,2,2,2,1,4,5]
-- sort 
-- nub
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) =[x : s | s <- powerSet xs] ++ powerSet xs 

-- code.hs:51:13: error:
-- • Expecting one more argument to ‘[]’
-- Expected a type, but ‘[]’ has kind ‘* -> *’
-- • In the type signature:
-- powerSet :: [] -> [[]]

div2 :: [Int]
div2  =take 100 [x | x <- [1..], x `mod` 2 == 0]

div3 :: [Int]
div3  =take 100 [x | x <- [1..], x `mod` 3 == 0]

union' :: [Int] -> [Int] -> [Int]
union' seta setb = [x | x <- [1..], x `elem` seta || x `elem` setb]

intersection' :: [Int] -> [Int] -> [Int]
intersection' seta setb = [x | x <- [1..], x `elem` seta && x `elem` setb]

difference :: [Int] -> [Int] -> [Int]
difference seta setb = [x | x <- [1..], x `elem` seta && x `notElem` setb]

complement' :: [Int] -> [Int] 
complement' x = [s | s <- [1..], s `notElem` x] 




