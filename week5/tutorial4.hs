import Data.Char

data Literal a = N a | P a
data Clause a  = Or [Literal a]
type Form a    = [ Clause a ]

neg :: Literal a -> Literal a
neg (P a) = N a
neg (N a) = P a

data Atom = A|B|C|D|W|X|Y|Z deriving Eq

eg = [ Or[N A, N C, P D], Or[P A, P C], Or[N D]]

data Val a = And [Literal a]
vals :: [a] -> [Val a]
vals atoms =
     let
        vs []     = [ [ ] ]
        vs (a:as) = [ P a: v | v <- vs as ] ++
                    [ N a: v | v <- vs as ]
     in map And (vs atoms)

v :: Eq a => Literal a -> (Val a -> Bool)
v lit (And gamma) = lit `elem` gamma

(|-) :: Eq a => Val a -> Clause a -> Bool
And gamma |- Or delta = or [ d `elem` gamma | d <- delta]

(|-/) :: Eq a => Val a -> Clause a -> Bool
And gamma |-/ Or delta = and [neg d `elem` gamma | d <- delta]