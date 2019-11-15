-- Informatics 1 - Introduction to Computation
-- Computation and Logic Tutorial 8
--
-- Week 9 (11-17 Nov.)
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

module CL8 where
import Prelude hiding (elem)
import Data.List(nub, sort)
import Test.QuickCheck
import Data.Char

-- Type declarations

type Sym = Char
type Trans q = (q, Sym, q)
--   FSM states symbols transitions starting accepting 
data FSM q = FSM [q] [Sym] [Trans q] [q] [q] deriving Show 
mkFSM qs as ts ss fs =
  FSM (set qs) (set as) (set ts) (set ss) (set fs)

-- DFA
isDFA :: Ord q => FSM q -> Bool
isDFA (FSM qs as ts ss fs) =
  (length ss == 1) && and[length[ q' | q' <- qs, (q, a, q')`elem`ts ] == 1
                         | q <- qs, a <- as ]
  
-- applying transitions for a given symbol to move a set of states
move :: (Ord q) => [q] -> [Trans q] -> Sym -> [q] -> [q] 
move qs ts x ss = --[ q' | (q, y, q') <- ts, x == y, q`elem`ss ]
   [ q' | q' <- qs, or[(q,x,q')`elem`ts| q <- ss] ]

-- moving the start states 
step :: Ord q => FSM q -> Sym -> FSM q
step (FSM qs as ts ss fs) x = FSM qs as ts (move qs ts x ss) fs

trace :: Ord q => FSM q -> [Sym] -> [[q]]
trace     (FSM _ _ _ ss _) []     = [ss]
trace fsm@(FSM _ _ _ ss _) (x:xs) = ss : trace (step fsm x) xs

accepts :: (Ord q) => FSM q -> String -> Bool
accepts (FSM _ _ _ [] _) _  = False
accepts (FSM _ _ _ ss fs) "" = or[ q`elem`ss | q <- fs ]
accepts fsm (x : xs) = accepts (step fsm x) xs

-- Examples
-- Q 3.

charFSM :: Char -> FSM Bool
charFSM c =
  mkFSM [False,True] [c] [(False,c,False),(True, c, True),
        (False, c, True), (True, c, False)] [False] [True]

stringFSM :: String -> FSM Int
stringFSM xs =
  mkFSM [0..n] xs [(0,xs!!7,7)] [0] [n]
  where n = length xs

emptyFSM :: FSM Int
emptyFSM = FSM [0] "" [(0,' ',0)] [0] [0]

nullFSM :: FSM ()
nullFSM = FSM [()] "" [] [()] [()]

-- Q 4.

data EG = Q0|Q1|Q2|Q3 deriving (Ord,Eq,Show)
[a,b,c,d] = "abcd"
simpleFSM = mkFSM qs as ts ss fs
 where
   qs = [Q0,Q1,Q2]
   as = [a,b,c,d]
   ts = [(Q0,a,Q1), (Q1,b,Q1),(Q1,c,Q2),(Q1,d,Q2)]
   ss = [Q0]
   fs = [Q2]

simpleFSMbh = mkFSM qs as ts ss fs
 where
   qs = [Q0,Q1,Q2,Q3]
   as = [a,b,c,d]
   ts = [(Q0,a,Q1),(Q1,b,Q1),(Q1,c,Q2),(Q1,d,Q2),
         (Q1,a,Q3),(Q0,b,Q3),(Q0,c,Q3),(Q0,d,Q3),
         (Q2,a,Q3),(Q2,b,Q3),(Q2,c,Q3),(Q2,d,Q3),
         (Q3,a,Q3),(Q3,b,Q3),(Q3,c,Q3),(Q3,d,Q3)]
   ss = [Q0]
   fs = [Q2]

-- Q 5
eg1 = mkFSM [0..8] as
  [(0,d,2),(0,b,6),(1,h,5),(2,i,5),(2,o,8),(3,t,0),(3,t,1),(4,e,1)
  ,(5,c,4),(5,o,8),(6,c,7),(6,e,8),(7,a,3),(8,d,4),(8,g,7)]
  [0,6] [1,7]
  where as@[a,b,c,d,e,g,h,i,o,t]="abcdeghiot"

muWords = mkFSM [0..8] as
  [(0,b,2),(2,u,1),(0,a,3),(3,h,6),(3,c,1),
   (6,k,9),(9,d,8),(8,i,5),(6,m,7),(8,o,7),
   (7,g,4),(1,t,4),(4,e,5)] [0,2,3,9] [1,4]
  where as@[a,b,c,d,e,g,h,i,o,t,u,k,m]="abcdeghiotukm" -- edit this as required
  --only inputs "dog,act,but"

-- Q 6

eg5i = mkFSM
       [0..5]
       "ab"
       [(0,a,1),(1,b,3),(3,b,4),
        (4,a,5),(5,a,1),(5,b,3)]
       [0]
       [4]

eg5ii = mkFSM
        [0..5]
        "ab"
        [(0,a,1),(1,b,3),(3,b,4),
        (4,a,5),(1,a,5),(5,b,3)]
        [0]
        [4]

eg5iii = mkFSM
       [0..5]
       "ab"
       [(0,a,1),(1,b,3),(3,b,4),
        (4,a,5),(5,a,1),(5,a,3)]
        [0]
        [1,4]

eg5iv = mkFSM
       [0..5]
       "ab"
       [(0,a,1),(1,b,3),(3,b,4),
        (4,a,5),(5,a,3),(5,a,1)]
       [0,5]
       [1,4]

eg5v = mkFSM
       [0..5]
       "ab"
       [(0,a,1),(1,b,3),(3,b,4),
        (4,a,5),(5,b,1),(5,a,3),(4,b,4)]
        [0]
        [1]
       

eg5vi = mkFSM
       [0..5]
       "ab"
       [(0,b,1),(1,a,3),(3,b,4),
        (4,b,4),(4,a,5),(5,a,1)]
       [0]
       [1]


        
-- Q 9
reverseFSM :: Ord q => FSM q -> FSM q
reverseFSM (FSM qs as ts ss fs) =
  mkFSM qs as ts ss [a | a<-qs, not (a `elem` fs)]

prop_reverse s = accepts (reverseFSM $ stringFSM s) (reverse s)

    
-- -- QuickCheck

safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)





-- sets as ordered lists
type Set a = [a]
set :: Ord a => [a] -> [a]
set = nub.sort
insert :: Ord a => a -> Set a -> Set a
insert x [] = [x]
insert x (y:ys) | x < y = x : y : ys
                | x == y = y : ys
                | x > y = y : insert x ys
                          
(\/) :: Ord a => Set a -> Set a -> Set a
xs@(x:xt) \/ ys@(y:yt) =
  case compare x y of
   LT -> x : (xt \/ ys)
   EQ -> x : (xt \/ yt)
   GT -> y : (xs \/ yt)
[] \/ ys = ys
xs \/ [] = xs


(/\) :: Ord a => Set a -> Set a -> Set a
xs@(x:xt) /\ ys@(y:yt) =
  case compare x y of
   LT -> (xt \/ ys)
   EQ -> x : (xt \/ yt)
   GT -> (xs \/ yt)
_ /\ _ = []

(\\) :: Ord a => Set a -> Set a -> Set a
xs@(x:xt) \\ ys@(y:yt) =
  case compare x y of
   LT -> x: (xt \\ ys)
   EQ -> (xt \\ yt)
   GT -> (xs \\ yt)
[] \\ _  = []
xs \\ [] = xs

elem :: Ord a => a -> Set a -> Bool
a`elem`[] = False
a`elem`xs@(x:xt) =
  case compare a x of
   LT -> False
   EQ -> True
   GT -> a`elem`xt

delete :: Ord a => a -> Set a -> Set a
delete x ys@(y:yt) =
  case compare x y of
   LT -> ys
   EQ -> yt
   GT -> y : delete x yt
   
prop_sorted xs = and [ x < y | (x,y) <- zip xs (tail xs) ]  
-- some properties needed to check reverse intersect complement
prop_string s  = accepts (stringFSM s) s

prop_invariant (FSM qs as ts ss fs) = 
  prop_sorted qs
  && prop_sorted as
  && prop_sorted ts
  && prop_sorted ss
  && prop_sorted fs


---- Stuff we may not need for thiis tutorial
m1 :: FSM Int
m1 = mkFSM
     [0,1,2,3,4] -- states
     "ab"    -- alphabet
     [ (0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2), (1,'b',4)
     , (2,'a',3), (2,'b',3), (3,'b',4), (4,'a',4), (4,'b',4) ]
     [0]  -- start
     [4]  -- accept

m2 :: FSM Char
m2 = mkFSM
     "ABCD"          -- states
     "01"        -- symbols
     [('A', '0', 'D'), ('A', '1', 'B'), ('B', '0', 'A'), ('B', '1', 'C'),
      ('C', '0', 'B'), ('C', '1', 'D'), ('D', '0', 'D'), ('D', '1', 'D')]
     "B"   -- start
     "ABC" -- accept

dm1 :: FSM [Int] 
dm1 = mkFSM 
      [[],[0],[1,2],[3],[3,4],[4]] -- states
      "ab"                     -- symbols
      [([],   'a',[]),    ([],   'b',[])
      ,([0],  'a',[1,2]), ([0],  'b',[1,2])
      ,([1,2],'a',[3]),   ([1,2],'b',[3,4])
      ,([3],  'a',[]),    ([3],  'b',[4])
      ,([3,4],'a',[4]),   ([3,4],'b',[4])
      ,([4],  'a',[4]),   ([4],  'b',[4])]
      [[0]]       -- start
      [[3,4],[4]] -- accept


-- --17.

star :: (Ord q) => FSM q -> FSM q
star  (FSM qs as ts ss fs) =
  FSM qs as 
   (ts \/ (set [(x, a, y) | x <-fs, a <- as, (z,a,y) <- ts, z`elem`ss ]))
   (ss \/ fs)
   (ss \/ fs)

prop_star :: String -> Int -> Bool
prop_star m n =
  prop_invariant fsm &&
  accepts fsm (concat (replicate i s))
  where
  fsm = star (stringFSM s)
  s = safeString m
  i = abs n
