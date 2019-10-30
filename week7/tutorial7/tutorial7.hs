-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 7
--
-- Week 7(29 Oct.- 01 Nov.)

-- module Main where

import LSystem
import Test.QuickCheck

pathExample = (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#:  Go 30)

-- 1a. split
split :: Command -> [Command]
split Sit = []
split (a :#: b) = split a ++ split b
split a = [a]

-- 1b. join
join :: [Command] -> Command
join = foldr (:#:) Sit 


join1 :: [Command] -> Command
join1 = foldr1 (:#:)  
--join [] = Sit
--join (x:xs)= x :#: join xs

-- eliminate Sit, exclude Sit
eliminateSit :: Command -> Command
eliminateSit x = join1 (split x)
-- 1c. equivalent
-- equivalent
equivalent a b = split a == split b

-- 1d. testing join and split
-- prop_split_join 
prop_split_join c = join (split c) == c

-- prop_split
prop_split c = undefined

-- 2a. copy
copy :: Int -> Command -> Command
--copy num str = join1 $ split $ join1 $ replicate num str
copy num str = join1 [str | x <- [1..num]]  

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon a = copy 5 (Go a :#: Turn 72.0 )

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon ds a = copy a (Go ds :#: Turn abc)
  where
    abc = fromIntegral (360 `div` a) :: Float


-- 3. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral side 0 step angle = Sit
spiral side n step angle 
     | side >= 0 && 
       step >= 0 = eliminateSit $ (Go side :#: Turn angle) :#:  spiral (side + step) (n-1) step angle
     | otherwise = error "Side and Step are negative numbers"

-- 4. optimise
-- Remember that Go does not take negative arguments.
optimise :: Command -> Command
optimise x = filter0 $ Go (sum[ valueGo a |a <- split x]) :#: Turn (sum [valueTurn a | a <- split x])
  where
   valueGo :: Command -> Distance      
   valueGo (Go a) 
        | a >= 0 = a
        | otherwise = error "Go Distance cannot be negative."
   valueGo (Turn a) = 0
   valueTurn :: Command -> Distance
   valueTurn (Turn a) = a   
   valueTurn (Go a) = 0
   filter0 :: Command -> Command
   filter0 (Go a :#: Turn b) 
          | b == 0 = Go a
          | a == 0 = Turn b
          | a == 0 && b == 0  = Sit
          | otherwise = Go a :#: Turn b
 
-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead = undefined


-- 6. snowflake
snowflake :: Int -> Command
snowflake = undefined


-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined

--------------------------------------------------
--------------------------------------------------
---------------- Optional Material ---------------
--------------------------------------------------
--------------------------------------------------

-- Bonus L-Systems

peanoGosper = undefined


cross = undefined


branch = undefined

thirtytwo = undefined

main :: IO ()
main = display pathExample