-- Informatics 1 - Introduction to Computation
-- Computation and Logic Tutorial 7
--
-- Week 6(21-25 Oct.)
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
module CL7 where

import Data.List (nub, delete, sortOn, minimumBy)
import Data.Sort(uniqueSort)
import Data.Ord (comparing)
import Control.Monad( liftM, liftM2 )
import Test.QuickCheck( quickCheck, withMaxSuccess,
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )

-- Propositional logic in Haskell----

-- The datatype Wff a
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

data Atom = A|B|C|D|W|X|Y|Z deriving (Eq, Show, Ord)
-- we will use these as propositional letters in examples
wff1  =  (V A :|: V B) :&: (V A :&: V B)
wff2  =  (V A :&: (V B :|: V C)) :&: 
       ((Not (V A) :|: Not (V B)) :&: (Not (V A) :|: Not (V C)))
wff3  =  (V A :->: V B) :&: (V A :&: Not (V B))
wff4  =  (V A :<->: V B) :&: ((V A :&: Not (V B)) :|: (Not (V A) :&: V B))

type Env a = [(a, Bool)]
lookUp :: Eq a => Env a -> a -> Bool
lookUp v x = the [ b | (x', b) <- v, x == x' ]
    where the [z] = z
          the []  = error ("valuation undefined")
          the _  = error ("multiple values")
-- we represent valuations abstractly as functions.
-- The code above generates such a function from an association list.


-- -- We give different implementations of substitute, evaluate, atoms ...
-- -- using the fact that they are all instances of a similar pattern
-- -- If you don't understand this, don't worry, the functions behave
-- -- exactly like the versions given in FP Tutorial 6

-- universal definition of functions from (a -> b) to (Wff a -> b)
-- for any b equipped with interpretations for T F Not :&: :|: and atoms
univ :: b ->             -- T
        b ->             -- F
        (b -> b) ->      -- not
        (b -> b -> b) -> -- and
        (b -> b -> b) -> -- or
        (a -> b) ->      -- action on atoms
        (Wff a -> b)     -- the result
univ t f n aa oo atm = 
  let ev T = t
      ev F = f
      ev (Not p)     = n (ev p)
      ev (p :&: q)   = ev p `aa` ev q
      ev (p :|: q)   = ev p `oo` ev q
      ev (p :->: q)  = ev (Not p :|: q)
      ev (p :<->: q) = ev ((Not p :|: q) :&: (p :|: Not q))
      ev (V a)       = atm a
  in ev

atoms :: Ord a => Wff a -> [a]
atoms      = univ [] [] id (\/) (\/) pure -- (\x -> [x])
  where -- represent sets as ordered lists \/ is union
    (\/) :: Ord a => [a] -> [a] -> [a]
    [] \/ ys = ys
    xs \/ [] = xs
    xs@(x:xt) \/ ys@(y:yt) =
      case compare x y of
       LT -> x : (xt \/ ys)
       EQ -> x : (xt \/ yt)
       GT -> y : (xs \/ yt)

-- note that this eliminates implications
substitute :: (a -> b) -> Wff a -> Wff b
substitute = univ T F Not (:&:) (:|:) . (V.)

evaluate :: Wff Bool -> Bool
evaluate   = univ True False not (&&) (||) id

join :: Wff (Wff a) -> Wff a
join = univ T F Not (:&:) (:|:) id

type Pred u = u -> Bool
top _ = True
bot _ = False
(&:&) p q x = p x && q x
(|:|) p q x = p x || q x

interpret :: (a -> Pred u) -> Wff a -> Pred u
interpret  = univ top bot (not.) (&:&) (|:|) -- as operations on predicates

-- evaluates a wff in a given environment
eval :: Eq a => Env a -> Wff a -> Bool
eval v wff = evaluate ( substitute (lookUp v) wff )

-- creates all possible truth assignments for a set of atoms
envs :: [a] -> [Env a]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a wff is satisfiable
satisfiable :: Ord a => Wff a -> Bool
satisfiable p  =  or [ eval e p | e <- envs (atoms p) ]

models :: Ord a => Wff a -> [Env a]
models p = [ e | e <- envs (atoms p),  eval e p ]

--------------------------------------------------

tautology :: Ord a => Wff a -> Bool
tautology p  =  and [ eval e p | e <- envs (atoms p) ]

prop_taut1 :: Ord a => Wff a -> Bool
prop_taut1 p  =  tautology p || satisfiable (Not p)

prop_taut2 :: Ord a => Wff a -> Bool
prop_taut2 p  =  not (satisfiable p) || not (tautology (Not p))

prop_taut :: Ord a => Wff a -> Bool
prop_taut p  =  tautology p == not (satisfiable (Not p))

-- 7.
equivalent :: Ord a =>  Wff a -> Wff a -> Bool
equivalent p q  =  and [eval e p == eval e q | e <- theEnvs]
    where
      theAtoms  =  nub (atoms p ++ atoms q)
      theEnvs   =  envs theAtoms

-- 8.
subformulas :: Eq a => Wff a -> [Wff a]
subformulas (Not p)      = Not p : subformulas p
subformulas (p :|: q)    = (p :|: q)   : nub (subformulas p ++ subformulas q)
subformulas (p :&: q)    = (p :&: q)   : nub (subformulas p ++ subformulas q)
subformulas (p :->: q)   = (p :->: q)  : nub (subformulas p ++ subformulas q)
subformulas (p :<->: q)  = (p :<->: q) : nub (subformulas p ++ subformulas q)
subformulas p            = [p]

equivalent' :: Ord a => Wff a -> Wff a -> Bool
equivalent' p q  =  tautology (p :<->: q)

prop_equivalent :: Ord a =>  Wff a -> Wff a -> Bool
prop_equivalent p q  =  equivalent p q == equivalent' p q

--------------------------------------------------
--------------------------------------------------
-- Optional Material
--------------------------------------------------
--------------------------------------------------

-- 10.
-- check for negation normal form
isNNF :: Wff a -> Bool
isNNF (Not (V _))     = True
isNNF (Not _)         = False
isNNF (p :&: q)       = isNNF p && isNNF q
isNNF (p :|: q)       = isNNF p && isNNF q
isNNF (_ :->: _)      = False
isNNF (_ :<->: _)     = False
isNNF _               = True -- (V a), T, F

-- 11.
-- convert to negation normal form
impElim :: Wff a -> Wff a
impElim (Not p)     = Not (impElim p)
impElim (p :|: q)   = impElim p          :|: impElim q
impElim (p :&: q)   = impElim p          :&: impElim q
impElim (p :->: q)  = Not (impElim p)    :|: impElim q
impElim (p :<->: q) = impElim (p :->: q) :&: impElim (q :->: p)
impElim  x          = x -- (V a), T, F
--impElim = substitute id

toNNF :: Wff a -> Wff a
toNNF (Not T)         = F
toNNF (Not F)         = T
toNNF (Not (Not p))   = toNNF p
toNNF (Not (p :&: q)) = toNNF (Not p) :|: toNNF (Not q)
toNNF (Not (p :|: q)) = toNNF (Not p) :&: toNNF (Not q)
toNNF (p :&: q)       = toNNF p :&: toNNF q
toNNF (p :|: q)       = toNNF p :|: toNNF q
toNNF  p              = p -- (V a), (Not (V a)), T, F

-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Wff Atom -> Bool
prop_NNF1 p  =  isNNF ((toNNF.impElim) p)

-- check if result of toNNF is equivalent to its input
prop_NNF2 ::  Wff Atom -> Bool
prop_NNF2 p  =  equivalent p ((toNNF.impElim) p)

-- 12.
-- check whether a formula is in conj. normal form
isCNF ::  Eq a => Wff a -> Bool
isCNF T          =  True
isCNF F          =  True
isCNF (p :&: q)
  | p == T || p == F || q == T || q == F = False
  | otherwise                            = isCNF p && isCNF q
isCNF p = isDisjLit p
    where 
      isDisjLit (p :|: q)  =  isDisjLit p && isDisjLit q
      isDisjLit p          =  isLit p
          where
            isLit (Not (V _))  =  True
            isLit (V _)        =  True
            isLit _            =  False

-- transform an arbitrary formula into a list of lists
toCNFList ::  Eq a => Wff a -> [[Wff a]]
toCNFList p = cnf ((toNNF.impElim) p) 
    where
      cnf F           =  [[]]
      cnf T           =  []
      cnf (V n)       =  [[V n]]
      cnf (Not (V n)) =  [[Not (V n)]]
      cnf (p :&: q)   =  nub (cnf p ++ cnf q)
      cnf (p :|: q)   =  [nub $ x ++ y | x <- cnf p, y <- cnf q]
      cnf _           = error ("toCNFList : argument not in NNF")


----------------------- Clausal Forms -----------------------------------
-----------Clausal Forms------------------------------------------
-- We will be using a particular represntation of CNF ------------
-- Literals are positive or negative atoms -----------------------
data    Literal a = N a | P a
                  deriving (Ord, Eq, Show)
newtype Clause a  = Or  [Literal a]
                  deriving (Ord, Eq, Show)
newtype Form a    = And [Clause a]
                  deriving (Eq, Show)
type Val a        = [Literal a] -- must be consistent       
-- a (partial) valuation is a consistent list of literals  --------
-- it cannot contain both P a and N a                      --------
-- we say it asserts each literal in the list              --------

(<&&>) :: Form a -> Form a  -> Form a  
And xs <&&> And ys =  And( xs ++ ys )
                                                          
neg :: Literal a -> Literal a
neg (P a) = N a
neg (N a) = P a

canonical :: Ord a => Form a -> Form a
canonical (And cs) = (And . uniqueSort) $
                     map (\ (Or xs) -> Or(uniqueSort xs)) cs
                     
-- The function wffToForm converts                         --------
-- the Wffs  from FP 6 into this setting                   --------
-- 1: complete this function
wffToForm :: Ord a => Wff a -> Form a
wffToForm = canonical.toForm.toCNFList where
  toForm :: [[Wff a]] -> Form a
  toForm x            = And (map toClause x)
  toClause x          = Or (map toLit x)
  toLit (Not(V a) )   = N a
  toLit (V a)         = P a
  toLit _             = error "expected Literal"

-- 2: Use quickCheck to check that a Wff is satisfiable
-- iff the corresponding Form it has a model
prop_form_equiv wff = satisfiable wff == determine(dpll(wffToForm wff))
  where
  determine a = length(a) > 0 
          
----------------------------
(<<) :: Eq a => [Clause a] -> Literal a -> [Clause a] 
cs << x = [ Or (delete (neg x) ys) | Or ys <- cs, not $ x `elem` ys ]

dpll :: Eq a => Form a -> [[Literal a]]
dpll (And css) =
  let models [] = [[]]
      models cs = case prioritise cs of
        Or []          -> []   -- empty clause: no models
        Or [lit]       -> [    lit : m | m <- models (cs << lit)] -- unit clause
        Or (lit : _)   -> [    lit : m | m <- models (cs << lit)]
                          ++
                          [neg lit : m | m <- models (cs << neg lit)]
  in models css
  where prioritise = minimumBy (comparing (\(Or lits) -> length lits))

--------------------------------------------------------------------
----- Tseytin generates CEF conjunctive equisatisfiable form -------
--------------------------------------------------------------------
-- 3: Complete the following function,
--    by replacing each occurrence of undefined
tseytinToForm p =
  let tt :: Eq a => Wff a -> [Clause (Wff a)]
      tt r@(Not a)     =
        [ Or[P r, P a], Or[N r, N a]] ++ tt a
      tt r@(a :&: b)   =
        [ Or[P r, N a, N b], Or[N r, P a]
        , Or[N r, P b]] ++ tt a ++ tt b
      tt r@(a :|: b)   = 
        [ Or[N r, P a, P b], Or[P r, N a]
        , Or[P r, N b]] ++ tt a ++ tt b
      tt r@(a :->:  b) = 
        [ Or[N r, N a, P b], Or[P r, P a]
        , Or[P r, N b]] ++ tt a ++ tt b
      tt r@(a :<->: b) = 
        [ Or[N r, N a, P b], Or[N r, P a, N b]
        , Or[P r, P a, P b], Or[P r, N a, N b]] 
        ++ tt a ++ tt b
      tt F             = [Or[N F]] -- we must make F False
      tt T             = [Or[P T]] -- we must make T True
      tt (V _)         = [] -- no further constraints
      -- we remove trivial clauses
      nontriv (Or xs) = and [ x /= neg x' | x <- xs, x' <- xs]
  in canonical . And $ ( Or[P p] : filter nontriv (tt p) )

-- 4: write a test to check that a Wff is satisfiable iff its
--    Tseytin transform has a model
prop_tseytin_equiv :: Wff Atom -> Bool
prop_tseytin_equiv wff = satisfiable wff /= null (dpll (tseytinToForm wff))

-- 5: use quickCheck to find a Wff, wff, for which
-- size (wffToForm wff) > 100 * size (tseytinToForm wff)
-- by replacing each occurrence of undefined
-- the name of our proposition gives a hint :-)
prop_tseytin_bigger :: Wff Atom -> Bool
prop_tseytin_bigger wff = size (wffToForm wff) < 10 * size (tseytinToForm wff)
  where size (And cs) = length $ dpll(And cs)

---------------------------------------------------------------------
-- For QuickCheck --------------------------------------------------------

instance Show a => Show (Wff a) where
    show  =  showWff

instance Arbitrary Atom where
  arbitrary = oneof $ map return [ A, B, C, D, W, X, Y, Z]

instance Arbitrary a => Arbitrary (Wff a) where
    arbitrary  =  sized wff
        where
          wff n | n <= 0     =  liftM V atom
                | otherwise  =  oneof [ liftM V atom
                                       , liftM Not subform
                                       , liftM2 (:|:)   subform subform
                                       , liftM2 (:&:)   subform subform
                                       , liftM2 (:->:)  subform subform
                                       , liftM2 (:<->:) subform subform
                                       ]
                 where
                   atom = arbitrary
                   subform   =  wff (n `div` 2)

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
          showsPrec 2 b )
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
    showsPrec _  F    = showChar '\x22A4'
    showsPrec _  T    = showChar '\x22A5'
    showsPrec p (a :&: b)
      = showParen (p>3)
         (showsPrec 3 a .showSpace .
          showChar '\x2227' . showSpace . 
          showsPrec 3 b )
    showsPrec p (a :|: b)
      = showParen (p>2)
         (showsPrec 2 a .showSpace .
          showChar '\x2228' . showSpace . 
          showsPrec 2 b )
    showsPrec p (a :->: b)
      = showParen (p>1)
         (showsPrec 1 a .showSpace .
          showChar '\x2B62' . showSpace . 
          showsPrec 2 b )
    showsPrec p (a :<->: b)
      = showParen (p>0)
         (showsPrec 0 a .showSpace .
          showChar '\x2B64' . showSpace . 
          showsPrec 0 b )
    showsPrec _ (Not a) = 
      showChar '\x00AC' . showsPrec 11 a
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

tables :: (Ord a, Show a) => [Wff a] -> IO ()
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
fullTable :: (Ord a, Show a) => Wff a -> IO ()
fullTable = tables . filter nontrivial . sortOn (length.atoms) . subformulas
  where nontrivial :: Wff a -> Bool
        nontrivial (Not(V _))   = False
        nontrivial (V _)        = False
        nontrivial T            = False
        nontrivial F            = False
        nontrivial _            = True
    
----------- For later

-- fmap :: (a -> b ) -> Wff a -> Wff b
instance Functor Wff where
  fmap = substitute

-- (<*>) :: Wff (a -> b ) -> Wff a -> Wff b
instance Applicative Wff where
  pure = V
  (<*>) pf pa = univ T F Not (:&:) (:|:) (<$>pa) pf

-- join = univ T F Not (:&:) (:|:) id          
-- (>>=) :: Wff a -> (a -> Wff b) -> Wff b
instance Monad Wff where
  (>>=) = flip (univ T F Not (:&:) (:|:))
