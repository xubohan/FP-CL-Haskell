-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 6
--
-- Week 6(21-25 Oct.)

import Data.List (nub, delete, sortOn)
import Data.Sort(uniqueSort, sort)
import Data.Maybe  (mapMaybe)
import Control.Monad( liftM, liftM2 )
import Debug.Trace
import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )

--------------------------------------------------
--------------------------------------------------
---------------- Warmup exercises ----------------
--------------------------------------------------
--------------------------------------------------

-- The datatype 'Fruit'

data Fruit = Apple String Bool
           | Orange String Int

-- Some example Fruit
apple, apple', orange :: Fruit
apple  = Apple "Granny Smith" False -- a Granny Smith apple with no worm
apple' = Apple "Braeburn" True     -- a Braeburn apple with a worm
orange = Orange "Sanguinello" 10    -- a Sanguinello with 10 segments

fruits :: [Fruit]
fruits = [Orange "Seville" 12,
          Apple "Granny Smith" False,
          Apple "Braeburn" True,
          Orange "Sanguinello" 10]

-- This allows us to print out Fruit in the same way we print out a list, an Int or a Bool.
instance Show Fruit where
  show (Apple variety hasWorm)   = "Apple("  ++ variety ++ "," ++ show hasWorm  ++ ")"
  show (Orange variety segments) = "Orange(" ++ variety ++ "," ++ show segments ++ ")"

-- 1.
isBloodOrange :: Fruit -> Bool
isBloodOrange (Apple _ _) = False
isBloodOrange (Orange x y)
                      | x == "Tarocco" ||
                        x == "Moro"    ||
                        x == "Sanguinello" = True
                      | otherwise          = False

-- 2.
bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments fruits= sum[ a x | x <- fruits, isBloodOrange x]
     where
     a :: Fruit -> Int
     a (Orange x y) = y

-- 3.
worms :: [Fruit] -> Int
worms fruits= sum [ 1 | x <- fruits, a x]
  where
  a :: Fruit -> Bool
  a (Apple _ x) = x
  a (Orange _ _) = False

--------------------------------------------------
--------------------------------------------------
---Implementing propositional logic in Haskell----
--------------------------------------------------
--------------------------------------------------

-- The datatype 'Wff' a
data Wff a = V a
            | T
            | F
            | Not (Wff a)
            | Wff a :|: Wff a
            | Wff a :&: Wff a
            | Wff a :->: Wff a
            | Wff a :<->: Wff a
            deriving (Eq, Ord)
infixr 3 :&:
infixr 2 :|:
infixr 1 :->:
infixr 0 :<->:

 
data Atom = A|B|C|D|P|Q|R|S|W|X|Y|Z deriving (Eq, Show, Ord)
-- we will use these as propositional letters in examples         
type Env a = [(a, Bool)]

lookUp :: Eq a => Env a -> a -> Bool
lookUp v x = the [ b | (x', b) <- v, x == x' ]
    where the [z] = z
          the []  = error ("valuation undefined")
          the zs  = error ("multiple values")
-- we represent valuations abstractly as functions.
-- The code above generates such a funcion from an association list.


-- Functions for handling Wffs
-- Use a function to substitute values for atoms
substitute :: (a -> b) -> Wff a -> Wff b
substitute _ T = T
substitute _ F = F
substitute f (Not p)     =  Not (substitute f p)
substitute f (p :|: q)   =  substitute f p :|: substitute f q
substitute f (p :&: q)   =  substitute f p :&: substitute f q
substitute f (p :->: q)  =  substitute f p :->: substitute f q
substitute f (p :<->: q) =  substitute f p :<->: substitute f q
substitute f (V a)  = V (f a)

-- evaluate a Wff whose atoms are Booleans
evaluate :: Wff Bool -> Bool 
evaluate T = True
evaluate F = False
evaluate (Not p)     = not (evaluate p)
evaluate (p :&: q)   = evaluate p && evaluate q
evaluate (p :|: q)   = evaluate p || evaluate q
evaluate (p :->: q)  = evaluate p <= evaluate q
evaluate (p :<->: q) = evaluate p == evaluate q
evaluate (V b)  = b

-- evaluates a wff in a given environment
eval :: Eq a => Env a -> Wff a -> Bool
eval v wff = evaluate ( substitute (lookUp v) wff )

-- list the atoms that occur in a wff - 
--  NOTE: atoms in the result must be unique
atoms :: Eq a => Wff a -> [a]
atoms (V x)        =  [x]
atoms (F)         = []
atoms (T)         = []
atoms (Not p)     = atoms p
atoms (p :|: q)   = nub (atoms p ++ atoms q)
atoms (p :&: q)   = nub (atoms p ++ atoms q)
atoms (p :->: q)  = nub (atoms p ++ atoms q)
atoms (p :<->: q) = nub (atoms p ++ atoms q)

-- creates all possible truth assignments for a set of atoms
envs :: [a] -> [Env a]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a wff is satisfiable
satisfiable :: Eq a => Wff a -> Bool
satisfiable p  =  or [ eval e p | e <- envs (atoms p) ]

-- models :: Eq a => Wff a -> [Env a]
-- models p = [ e | e <- envs (atoms p),  eval e p ]


--------------------------------------------------
--------------------------------------------------
------------------ Exercises ---------------------
--------------------------------------------------
--------------------------------------------------

-- 4.
wff1 = ((V P :|: V Q) :&: (V P :&: V Q))
wff2 = ((V P :&: (V Q :|: V R)) :&: (Not(V P) :|: Not(V Q)) :&: (Not(V P) :|: Not(V R)))

-- 5. 
tautology :: Eq a => Wff a -> Bool
tautology str = and [eval x str | x <- envs $ atoms str]


prop_taut1 :: Eq a => Wff a -> Bool
prop_taut1 p = tautology p || satisfiable (Not p) 

prop_taut2 :: Eq a => Wff a -> Bool
prop_taut2 p = not (satisfiable p) || not (tautology (Not p))

prop_taut :: Eq a => Wff a -> Bool
prop_taut p  = prop_taut1 p == prop_taut2 p

-- 6.
wff3 = ((V P :->: V Q) :&: (V P :&: Not (V Q)))
wff4 = ((V P :<->: V Q) :&: (V P :&: Not(V Q)) :|: (Not (V P) :&: V Q))

-- 7.
equivalent :: Eq a =>  Wff a -> Wff a -> Bool
equivalent a b = and [eval x a == eval x b| x <- envsList]
   where
   envsList = envs (nub (atoms a ++ atoms b))
   

-- 8.
subformulas :: Eq a => Wff a -> [Wff a]
subformulas (V a) =[V a]
subformulas (Not (V a)) = [Not (V a)]
subformulas (F) = [F]
subformulas (T) = [T]
subformulas (Not p) = nub( [Not p] ++ (subformulas p))
subformulas (a :&: b) = nub([(a :&: b) ] ++ subformulas a ++ subformulas b)
subformulas (a :|: b) = nub([(a :|: b) ] ++ subformulas a ++ subformulas b)
subformulas (a :->: b) = nub([(a :->: b) ] ++ subformulas a ++ subformulas b)
subformulas (a :<->: b) = nub( [(a :<->: b) ] ++ subformulas a ++ subformulas b)


-- 9.
wff5 = (V P :|: V Q) :&: (Not (V P) :&: Not (V P))

wff6 = (V P :->: V Q) :&: (V P :<->: V Q)

equivalent' :: Eq a => Wff a -> Wff a -> Bool
equivalent' a b = and [tautology ( a :<->: b) && satisfiable x | x <- subList]
   where
   subList = nub( subformulas a++ subformulas b)

-- if and only if (:<->:)
--  biconditional 


prop_equivalent :: Eq a =>  Wff a -> Wff a -> Bool
prop_equivalent a b= equivalent a b == equivalent' a b

--------------------------------------------------
--------------------------------------------------
-- Optional Material
--------------------------------------------------
--------------------------------------------------

-- 10.
-- check for negation normal form
isNNF :: Wff a -> Bool
isNNF (Not(V a :&: V b)) = True
isNNF (Not(V a :|: V b)) = True
isNNF (V a :->: V b)     = True
isNNF (V a :<->: V b)    = True
isNNF (Not(Not(V a)))    = True
isNNF _                  = False

-- 11.
-- convert to negation normal form
impElim :: Wff a -> Wff a
impElim (Not (a) :|: Not(b)) = (Not(a :&: b))
impElim (Not (a) :&: Not(b)) = (Not(a :|: b))
impElim (Not (a) :|: b) = (a :->: b) 
impElim (((a :->: b) :&: (b :->: a))) = (a :<->: b)
impElim  a = (Not(Not(a)))

toNNF :: Wff a -> Wff a
toNNF = undefined

-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Wff a -> Bool
prop_NNF1 f  =  isNNF (toNNF f)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Eq a => Wff a -> Bool
prop_NNF2 f  =  equivalent f (toNNF f)

-- 12.
-- check whether a formula is in conj. normal form
isCNF :: Eq a => Wff a -> Bool
isCNF = undefined

-- 13.
-- transform a list of lists into a (CNF) formula
listsToCNF :: [[Wff a]] -> Wff a
listsToCNF = undefined

-- 14.
-- transform a CNF formula into a list of lists
listsFromCNF :: Eq a => Wff a -> [[Wff a]]
listsFromCNF = undefined

-- 15.
-- transform an arbitrary formula into a list of lists
toCNFList :: Eq a => Wff a -> [[Wff a]]
toCNFList = undefined

-- convert to conjunctive normal form
toCNF :: Eq a => Wff a -> Wff a
toCNF wff  =  listsToCNF (toCNFList wff)

-- check if result of toCNF is equivalent to its input
prop_CNF :: Eq a => Wff a -> Bool
prop_CNF p  =  equivalent p (toCNF p)


----------- PRESENTATION -------------------------
showWff :: Show a => Wff a -> String
showWff e = showsPrec 0 e ""
    where
    showsPrec _ (V a) = showString (show a)
    showsPrec _  F    = showChar 'F'
    showsPrec _  T    = showChar 'T'
    showsPrec p (a :&: b)
      = showParen (p>3)
         (showsPrec 3 a .showSpace .
          showString "&" . showSpace . 
          showsPrec 3 b )
    showsPrec p (a :|: b)
      = showParen (p>2)
         (showsPrec 2 a .showSpace .
          showString "|" . showSpace . 
          showsPrec 2 b )
    showsPrec p (a :->: b)
      = showParen (p>1)
         (showsPrec 1 a .showSpace .
          showString "->" . showSpace . 
          showsPrec 1 b )
    showsPrec p (a :<->: b)
      = showParen (p>0)
         (showsPrec 0 a .showSpace .
          showString "<->" . showSpace . 
          showsPrec 0 b )
    showsPrec _ (Not a) = 
      showString "~" . showsPrec 11 a
    showString :: String -> String -> String
    showString = (++)
    showChar :: Char -> String -> String
    showChar   = (:)
    showSpace :: String -> String
    showSpace  = showChar ' '
    showParen :: Bool -> (String -> String) -> (String -> String)
    showParen p s = if p then showChar '(' . s . showChar ')' else s


pretty :: Show a => Wff a -> String
pretty e = showsPrec 0 e ""
  where
    showsPrec _ (V a) = showString (show a)
    showsPrec _  F    = showChar 'F'
    showsPrec _  T    = showChar 'T'
    showsPrec p (a :&: b)
      = showParen (p>3)
         (showsPrec 3 a .showSpace .
          showString "&" . showSpace . 
          showsPrec 3 b )
    showsPrec p (a :|: b)
      = showParen (p>2)
         (showsPrec 2 a .showSpace .
          showString "|" . showSpace . 
          showsPrec 2 b )
    showsPrec p (a :->: b)
      = showParen (p>1)
         (showsPrec 1 a .showSpace .
          showString "->" . showSpace . 
          showsPrec 1 b )
    showsPrec p (a :<->: b)
      = showParen (p>0)
         (showsPrec 0 a .showSpace .
          showString "<->" . showSpace . 
          showsPrec 0 b )
    showsPrec _ (Not a) = 
      showString "~" . showsPrec 11 a
    showString :: String -> String -> String
    showString = (++)
    showChar :: Char -> String -> String
    showChar   = (:)
    showSpace :: String -> String
    showSpace  = showChar ' '
    showParen :: Bool -> (String -> String) -> (String -> String)
    showParen p s = if p then showChar '(' . s . showChar ')' else s

-- For Drawing Tables ----------------------------------------------------

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
  where
    widths  = map length (head tab)

table p = tables [p]

tables :: (Eq a, Show a) => [Wff a] -> IO ()
tables ps  =
  let xs = nub (concatMap atoms ps) in
   showTable (
     [ map show xs ++ ["|"] ++ [pretty p | p <- ps]           ] ++
     [ dashvars xs ++ ["|"] ++ [dash (pretty p) | p <- ps ]   ] 
     ++   [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
     )
  where  dashvars xs   = [ dash (show x) | x <- xs ]
         evalvars e xs = [ fort (eval e (V x)) | x <- xs ]

 -- print a truth table, including columns for subformulas
fullTable :: (Eq a, Show a) => Wff a -> IO ()
fullTable = tables . filter nontrivial . sortOn (length.atoms) . subformulas
  where nontrivial :: Wff a -> Bool
        nontrivial (Not(V _))   = False
        nontrivial (V _)        = False
        nontrivial T            = False
        nontrivial F            = False
        nontrivial _            = True


-- For QuickCheck --------------------------------------------------------

instance Show a => Show (Wff a) where
    show  =  showWff

instance Arbitrary Atom where
  arbitrary = oneof $ map return [ A, B,  C, D, W, X, Y, Z]

instance Arbitrary a => Arbitrary (Wff a) where
    arbitrary  =  sized wff
        where
          wff n | n <= 0     =  liftM V atom
                | otherwise  =  oneof [ liftM V atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       , liftM2 (:&:) subform subform
                                       , liftM2 (:->:) subform subform
                                       , liftM2 (:<->:)subform' subform'
                                       ]
                 where
                   atom = arbitrary
                   subform  =  wff (n `div` 2)
                   subform' =  wff (n `div` 4)