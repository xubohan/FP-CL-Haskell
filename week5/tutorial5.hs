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
doubles = map (*2)

-- b.        
penceToPounds :: [Int] -> [Float]
penceToPounds a = map (*0.01) [fromIntegral x:: Float | x<- a]
  
-- c.
uppersComp :: String -> String
uppersComp a = [ toUpper x | x <- a ]

uppers :: String -> String
uppers = map toUpper

prop_upper :: String -> Bool
prop_upper x  = uppers x == uppersComp x

-- 2. Filter
-- a.
alphas :: String -> String
alphas = filter isAlpha

-- b. 
above :: Int -> [Int] -> [Int]
above key = filter (> key) 

-- c.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter (\(x,y)-> x /= y)

-- d.
rmCharComp :: Char -> String -> String
rmCharComp key = filter ( /= key) 

rmChar :: Char -> String -> String
rmChar key str =  [ x | x <- str, x /= key]

prop_rmChar :: Char -> String -> Bool
prop_rmChar key str = rmCharComp key str == rmChar key str


-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' x = map (*2) (filter (>3) x)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' str = map reverse (filter (\str -> even(length str)) str)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold xs= foldr ( && ) True  xs

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- b.
concatRec :: [[a]] -> [a]
concatRec [[ ]] = [ ] 
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold xs= foldr (\x acc -> x++acc) [ ] xs

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec k [ ] = [ ]
rmCharsRec k (x:xs)
           | elem x k  = rmCharsRec k xs 
           | otherwise = x : rmCharsRec k xs
           

rmCharsFold :: String -> String -> String
rmCharsFold k xs = foldr (rmCharComp) xs k


prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str


type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
--uniform (x:xs)= and (map (==x) xs)
uniform [ ] = False
uniform (x:xs) = all (==x) xs
 
-- b.
valid :: Matrix -> Bool
valid (x:xs)= uniform (map (length) (x:xs)) &&
              length x >= 1 && length (x:xs) >= 1


-- 6.
matrixWidth :: Matrix -> Int
matrixWidth m = head (map (length) m)

matrixHeight :: Matrix -> Int
matrixHeight m = length m

plusM :: Matrix -> Matrix -> Matrix
plusM a b = zipWith (zipWith (+)) a b

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM a b 
           |matrixWidth (a) /= matrixHeight(b) = error "not suitable for timesM"
           |otherwise = [ [dotProduct x y| y <- transpose b] | x <- a ]
              
dotProduct :: [Rational] -> [Rational] -> Rational
dotProduct a b = sum (zipWith (*) a b)


-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [f a b | (a,b) <- zip xs ys]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (\(x,y) -> uncurry f (x,y)) (zip xs ys) 

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