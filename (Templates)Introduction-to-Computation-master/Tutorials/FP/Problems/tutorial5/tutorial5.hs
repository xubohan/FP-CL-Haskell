-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 5
--
-- Week 5(14-18 Oct.)

module Tutorial5 where
import Data.List
import Data.Char
import Data.Ratio
import Test.QuickCheck

-- 1. Map

-- a.
doubles :: [Int] -> [Int]
doubles  = map double 
    where double xs = xs * 2 

-- b.        
penceToPounds :: [Int] -> [Float]
penceToPounds = map f
    where f x = fromIntegral x / 100

-- c.
uppersComp :: String -> String
uppersComp str = [toUpper c | c <- str]


-- 2. Filter
-- a.
alphas :: String -> String
alphas = filter isAlpha

-- b.
above :: Int -> [Int] -> [Int]
above limit =  filter (limit<)

-- c.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter unequal
    where unequal (x,y) = x /= y
-- d.
rmCharComp :: Char -> String -> String
rmCharComp ch str = [ch1 | ch1 <- str, ch1 /= ch]


-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map double (filter biggerThan3 xs)
    where biggerThan3 x = x > 3
          double x = x * 2

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = [reverse s | s <- strs, even (length s)]

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:xs) = x && andRec xs
-- 上面定义似乎 有什么问题。。。但是能运行
-- andRec xs = foldr (&&) True xs 
andFold :: [Bool] -> Bool
andFold xs = andRec xs == andFold xs 

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- b.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (xs:xss) = xs ++ concatRec xss
-- 完全等价
-- hlint看不下去了。。。
concatFold :: [[a]] -> [a]
concatFold xss = foldr (++) [] xss

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- c.
--似乎有什么问题
--rmCharsRec :: String -> String -> String
--rmCharsRec cs str = foldr rmChar str cs
-- 这里也是
--rmCharsFold :: String -> String -> String
--rmCharsRec cs str = foldr rmChar str cs

--prop_rmChars :: String -> String -> Bool
--prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str


type Matrix = [[Rational]]

-- 5
-- a.

uniform :: [Int] -> Bool
uniform xs = all (== head xs) (tail xs)
-- all 
-- tail 取队尾


-- b.
valid :: Matrix -> Bool
valid (x:xs) = not (null x) && uniform (map length (x:xs))
-- null 不要用 []


-- 6.
matrixWidth :: Matrix -> Int
matrixWidth m = length (head m)

matrixHeight :: Matrix -> Int
matrixHeight = length

plusM :: Matrix -> Matrix -> Matrix
plusM m n | ok        = zipWith (zipWith (+)) m n
          | otherwise = error "Invalid"
  where ok = matrixWidth m == matrixWidth n
             && matrixHeight m == matrixHeight n
             && valid m && valid n
-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2 | ok        = [ [ dot row col | col <- transpose m2 ] | row <- m1 ]
             | otherwise = error "Invalid input"
  where dot xs ys = sum (zipWith (*) xs ys)
        ok        = matrixWidth m1 == matrixHeight m2
-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [ f x y | (x,y) <- zip xs ys ]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' = zipWith
-- hlint认为这样是对的 测试无误 
-- -----------------------------------
-- -----------------------------------
-- -- Optional material
-- -----------------------------------
-- -----------------------------------
-- -- 9.

-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined