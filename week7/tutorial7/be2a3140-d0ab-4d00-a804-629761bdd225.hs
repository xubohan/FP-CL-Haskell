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
join [] = Sit
join (x:xs)= x :#: join xs

--join (x:xs)= foldr (:#:) x xs

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
copy num str = join (split (join(replicate (num - 1) str)) ++ (split str))
  where
    join1 [a] = a   
    join1 (x:xs)= x :#: join1 xs   

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon a = copy fromIntegral a:: Float (Go 50 :#: Turn 72 )

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon ds a = copy (fromIntegral a:: Float) (Go (fromIntegral ds:: Float) :#: Turn (fromIntegral (360/a) :: Float))


-- 3. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral = undefined


-- 4. optimise
-- Remember that Go does not take negative arguments.

optimise :: Command -> Command
optimise = undefined

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