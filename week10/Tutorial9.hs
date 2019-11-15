import Prelude hiding (elem, lookup)
import Data.List(nub, sort)
import Test.QuickCheck
import Data.Char

-- 1. return the set of nodes reached by any number of steps (including 0)
reach :: Ord q => ([q] -> Set q) -> Set q -> Set q
reach step gs =
   if gs' == gs then gs else reach step gs'
   where gs' = gs \/ step gs

-- Uncomment the block below and complete your answer
{-
reach :: Ord q => ([q] -> [q]) -> [q] -> Set q
reach step qs =
  let add q qss
        | q `elem` qss = qss
        | otherwise =
          foldr add undefined undefined
  in foldr add [] qs
-}

-- Labelled transitions
type Sym = Char
type Trans q = (q, Sym, q)




-- 2.
toy_ts = [ (n,intToDigit m, m*n `mod` 10) | n <- [0..9], m <- [3,7] ]

-- Uncomment the block below and complete your answer 
{-
smallstep :: [Int] -> [Int]
smallstep qs = set[... | (q,_,q') <- toy_ts, ... ]  
-}



-- 3.
-- one_step ts qs produces the states reached adjacent to qs
-- then can be reached by a single transition from ts 
one_step :: Ord q => [Trans q] -> [q] -> Set q
one_step ts qs = set undefined

--   FSM states symbols transitions starting accepting 
data FSM q = FSM (Set q) [Sym] [Trans q] (Set q) (Set q) deriving Show 
mkFSM qs as ts ss fs = FSM (set qs) as ts (set ss) (set fs)

-- toy examples
g0 :: [Int] -> [Int] -> FSM Int
g0 = mkFSM [0..9] "37" [ (n,intToDigit m, m*n `mod` 10) | n <- [0..9], m <- [3,7] ]
eg0 = g0 [1]   [9]
eg1 = g0 [1,2] [9]
-- 



-- 4.
reachableFSM (FSM _ _ ts ss _) = reach (one_step ts) ss

-- 5.Uncomment the block below and complete your answer 
pruneFSM fsm@(FSM qs as ts ss fs) =
  FSM qs' as ts' ss fs' where
    qs' = reachableFSM fsm
    ts' = undefined
    fs' = undefined




-- 6.
-- 
reverseFSM (FSM qs as ts ss fs) =
  FSM qs as ts' ss' fs' where 
    ts' = undefined
    ss' = undefined
    fs' = undefined


-- prop_reverse s = accepts (reverseFSM $ stringFSM s) (reverse s)



-- 7.
tidyFSM :: Ord q => FSM q -> FSM q
tidyFSM = undefined . undefined . undefined . undefined


-- NFA adding epsilon-transitions
data NFA q = NFA (Set q) [Sym] [Trans q] [(q,q)] (Set q)(Set q) deriving Show
mkNFA qs as ts es ss fs =
  NFA (set qs) as ts [ e | e@(q,q') <- es, q/=q'] (set ss) (set fs)
m3 :: NFA Int
m3 =  NFA [0..5] "ab" [ (1,'a',2), (3,'b',4)] [ (0,1), (2,3), (0,5), (4,5), (4,1)] [0] [5]



-- 8.  
eStep  :: Ord q =>  [(q,q)] -> [q] -> Set q
eStep es qs = undefined
eClose :: Ord q => [(q,q)] -> [q] -> Set q   
eClose es = reach (eStep es)




-- 9.
-- the action of a symbol s; reached by a single step                                
-- following only transitions with a given label (cf. ddelta)
ddelta :: (Ord q) => [Trans q] -> Sym -> [q] -> Set q
ddelta ts a = one_step undefined




-- 10.
eddelta :: (Ord q) => [Trans q] -> [(q,q)] -> Sym -> [q] -> Set q
eddelta ts es a =  undefined . undefined




-- next is like step, but on superstates
next :: Ord q =>  [Trans q] -> [Sym] -> [[q]] -> Set (Set q)
next ts as qqs = set [ddelta ts a qs | qs <- qqs, a <- as ]

fsm2dfa :: Ord q => FSM q -> FSM [q]
fsm2dfa (FSM qs as ts ss fs) =
  FSM qss as ts' ss' fs' where
  qss = reach (next ts as) [ss] 
  ts' = [ (qs, a, ddelta ts a qs) | qs <- qss, a <- as ]
  ss' = [ss]
  fs' = [ qs | qs <- qss, or[ q`elem`fs | q <- qs ]]




-- 11.
-- eNext is like next, but including e-transitions
eNext :: Ord q =>  [Trans q] -> [(q,q)] -> [Sym] -> [[q]] -> Set (Set q)
eNext ts es as qqs = set undefined



-- 12.
nfa2dfa :: Ord q => NFA q -> NFA [q]
nfa2dfa (NFA qs as ts es ss fs) =
  mkNFA qss as ts' es' ss' fs' where
  qss = undefined
  ts' = undefined
  es' = undefined
  ss' = undefined
  fs' = undefined



-- Utility functions to transform the transitions
mapTrans :: (q -> r) -> [Trans q] -> [Trans r]
mapTrans f ts = [ (f q,x,f q') | (q,x,q') <- ts ]

lookUp :: (Show q, Ord q) => [(q,Int)] -> q -> Int
lookUp qis q' = the [ i | (q,i) <- qis, q == q' ]
  where
  the [q] = q
  the _ = error (show qis)



-- 13
reverseNFA :: Ord q => NFA q -> NFA q
reverseNFA = undefined

tidyNFA :: Ord q => NFA q -> NFA [[q]]
tidyNFA =  nfa2dfa . reverseNFA .  nfa2dfa . reverseNFA



-- 14. Use mapTrans and lookUp for this exercise 
intFSM :: (Ord q, Show q) => FSM q -> FSM Int
intFSM (FSM qs as ts ss fs)  = undefined
    



------------------------------
-- regex -> DFA
------------------------------

isThompson (NFA qs as ts es [s] [f]) =
  null [ q | (q,_,q') <- ts, s==q'||f==q] && null [ q | (q,q') <- es, s==q'||f==q ]
isThompson _ = False

data QF q = Q | E q | F deriving (Eq, Ord)
thompson :: Ord q => NFA q -> NFA (QF q)
thompson (NFA qs as ts es ss fs) =
  mkNFA qs' as ts' es' ss' fs'
  where qs' = Q: F : map E qs
        ts'  = mapTrans E ts
        es'  = [ (E q, E q') | (q,q') <- es] 
               ++[ (Q,E q) | q <- ss ] ++ [(E q,F) | q <- fs ]
        ss'  = [Q]
        fs'  = [F]



-- 15.
stringNFA :: String -> NFA Int
stringNFA xs = undefined
        
nullNFA :: NFA Bool
nullNFA = undefined

fullNFA :: NFA Bool
fullNFA = undefined




-- 16.
concatNFA :: (Ord a, Ord b) => NFA a -> NFA b -> NFA (Either a b)
concatNFA (NFA qs as ts es ss fs)(NFA qs' as' ts' es' ss' fs') = undefined



-- 17.
kstarNFA :: NFA q -> NFA(QF q)
kstarNFA (NFA qs as ts es ss fs) = undefined



-- 18.
unionNFA :: (Ord q, Ord q') => NFA q -> NFA q' -> NFA (QF (Either q q'))
unionNFA (NFA qs as ts es ss fs) (NFA qs' as' ts' es' ss' fs') = undefined


-- 19
completeNFA :: (Ord q) => NFA q -> NFA (Maybe q)
completeNFA   = undefined

complementNFA :: (Ord q) => NFA q -> NFA (Maybe q)
complementNFA = undefined

intersectNFA :: (Ord q, Ord q') => NFA q -> NFA q' -> NFA (q,q')
intersectNFA  = undefined



-- 20
data Regex = Zero             -- empty language
           | Eps              -- [""]
           | S String          -- [s]
           | (:|:) Regex Regex -- R|S union
           | (:>:) Regex Regex -- RS
           | Star Regex        -- R*
           | Not Regex         -- complement
           | (:&:) Regex Regex -- intersection
           deriving (Show)

regex2NFA :: Regex -> NFA Int
regex2NFA = undefined

-- DFA
isDFA :: Ord q => FSM q -> Bool
isDFA (FSM qs as ts ss fs) =
  and[length[ q' | q' <- qs, (q, a, q')`elem`ts ] == 1| q <- qs, a <- as ]
  && (length ss == 1)

isComplete (FSM qs as ts ss fs) = and[or[(q,a,q')`elem` ts | q' <- qs] |q<-qs,a<-as]


-- Below are the code for operations on FSMs, Regex, set operations, and props for testing
-- moving the start states 
move :: Ord q => FSM q -> Sym -> FSM q
move (FSM qs as ts ss fs) x = FSM qs as ts (ddelta ts x ss) fs

trace :: Ord q => FSM q -> [Sym] -> [[q]]
trace     (FSM _ _ _ ss _) []     = [ss]
trace fsm@(FSM _ _ _ ss _) (x:xs) = ss : trace (move fsm x) xs

accepts :: (Ord q) => FSM q -> String -> Bool
accepts (FSM _ _ _ [] _) _  = False
accepts (FSM _ _ _ ss fs) "" = or[ q`elem`ss | q <- fs ]
accepts fsm (x : xs) = accepts (move fsm x) xs

--Basic FSMs

charFSM :: Char -> FSM Bool
charFSM c =
  mkFSM [False,True] [c] [(False,c,True)] [False] [True]

stringFSM :: String -> FSM Int
stringFSM xs =
  mkFSM [0..n] xs [(i,xs!!i,i+1) | i <- [0..n-1]] [0] [n]
  where n = length xs

emptyFSM :: FSM Int -- FSM Int to give this as instance of stringFSM
emptyFSM = stringFSM ""

nullFSM :: FSM ()
nullFSM = mkFSM [] [] [] [] []

-- operations directly on FSM
sumFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')
sumFSM (FSM qs as ts ss fs) (FSM qs' as' ts' ss' fs') =
  mkFSM
  (map Left qs \/ map Right qs')
  (as \/ as')
  (mapTrans Left ts \/ mapTrans Right ts')
  (map Left ss \/ map Right ss')
  (map Left fs \/ map Right fs')

productFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (q,q')
productFSM (FSM qs as ts ss fs) (FSM qs' as' ts' ss' fs')
  = FSM 
    [( x, y ) | x <- qs, y <- qs' ]
    (as \/ as')
    (set[((x,y), a, (x',y')) | (x,a,x') <- ts, (y,a',y') <- ts', a == a'])
    [ (x,y) | x <- ss, y <- ss' ]
    [ (x,y) | x <- fs, y <- fs' ]

intersectFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (q,q')
intersectFSM = productFSM

completeFSM :: Ord q => FSM q -> FSM (Maybe q)
completeFSM (FSM qs as ts ss fs) = -- add black hole state
  FSM
  (Nothing : map Just qs)
  as
  (mapTrans Just ts \/
   [ (Nothing, x, Nothing) | x <- as ] \/
   [ (Just q,x,Nothing) | q <- qs, x <- as
                        , and[ r/=q || x/= y | t@(r,y,_) <- ts]])
  (map Just ss)
  (map Just fs)

complementFSM :: Ord q => FSM q -> FSM (Maybe q)
complementFSM fsm =
  let (FSM qs as ts ss fs) = completeFSM fsm
  in FSM qs as ts ss (qs \\ fs)


(&) :: a -> (a -> b) -> b
(&) x f = f x -- "a" 
infix 9 & 
infix 8 :>:
infix 4 :&:
infix 3 :|:

-- -- QuickCheck

safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

-- THESE ARE HERE TO ENABLE CHECKING

concatFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')
concatFSM (FSM qs as ts ss fs)  (FSM qs' as' ts' ss' fs') = 
  FSM (map Left qs \/ map Right qs')
  (as \/ as')
  ([(Left q, sym, Right r') | q <- fs, (r,sym,r') <- ts', r`elem`ss'] \/
   mapTrans Left ts \/ mapTrans Right ts')
  (map Left ss)
  (map Left (if or[ q `elem` fs'| q <- ss' ] then fs else [])
   \/ map Right fs')
  
prop_concatFSM :: String -> String -> String -> Bool
prop_concatFSM m n o =
  accepts fsm (s ++ t)
  && (accepts fsm u == (s ++ t == u))
  where
  fsm = concatFSM a b
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o

prop_stringFSM m n =
  accepts a s
  && (accepts a t == (s == t))
  where
  a = stringFSM s
  s = safeString m
  t = safeString n

unionFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')
unionFSM  (FSM qs as ts ss fs) (FSM qs' as' ts' ss' fs')
  = FSM 
    (map Left qs \/ map Right qs')
    (as \/ as')
    (mapTrans Left ts \/ mapTrans Right ts')
    (map Left ss \/ map Right ss')
    (map Left fs \/ map Right fs')
        
prop_unionFSM :: String -> String -> String -> Bool
prop_unionFSM m n o =
  accepts fsm u == (accepts a u || accepts b u)
  && accepts fsm s
  && accepts fsm t
  where
  fsm = unionFSM a b
  a = stringFSM s
  b = stringFSM t
  c = stringFSM u
  s = safeString m
  t = safeString n
  u = safeString o




-- sets as ordered lists
type Set a = [a]
set :: Ord a => [a] -> [a]
set = nub.sort

insert :: Ord a => a -> Set a -> Set a
insert x ys@(y:yt) =
    case compare x y of
     LT -> x : ys
     EQ -> ys
     GT -> y : insert x yt
insert x [] = [x]
                          
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
   LT -> (xt /\ ys)
   EQ -> x : (xt /\ yt)
   GT -> (xs /\ yt)
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
   
prop_sorted xs = and [ x < y | (x,y) <- zip xs (tail xs) ]  
-- some properties needed to check reverse intersect complement
prop_string s  = accepts (stringFSM s) s

prop_invariantFSM (FSM qs as ts ss fs) = 
  prop_sorted qs
  && prop_sorted as
  && prop_sorted ts
  && prop_sorted ss
  && prop_sorted fs



