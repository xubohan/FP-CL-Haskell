-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 8
--
-- Week 8(04-08 Nov.)

module Tutorial8 where

import System.Random


-- Importing the keymap module

import KeymapTree

-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen x = maximum [ numbers [y] | y <- x]
    where
      numbers :: [(Barcode, Item)] -> Int
      numbers [(a,(b,c))] = length b

formatLine :: Int -> (Barcode, Item) -> String
formatLine x (a,(b,c))
            | length b == x   = a ++ "..." ++ b ++ "..." ++ c
            | otherwise       = a ++ "..." ++ b ++ 
                                replicate (x - length b)'.' 
                                ++ "..." ++ c

showCatalogue :: Catalogue -> String
showCatalogue x =  sC toListx
      where
        toListx = toList x
        showUnit (a,(b,c)) = "[("++ a ++ "," ++ b ++ 
                               "," ++ c ++ ")]"
        sC [] = []
        sC (y:ys) = showUnit y ++ sC ys
     
-- Exercise 2

--get "9780201342758" testDB
--Just ("Thompson - \"Haskell: The Craft of Functional Programming\"","Book")
--get "000" testDB
--Nothing

maybeToList :: Maybe a -> [a]
maybeToList x = case x of 
  Nothing -> []
  Just a  -> [a]

listToMaybe :: [a] -> Maybe a
listToMaybe x = case x of 
  []      -> Nothing
  str     -> Just (head str)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = maybeToList x ++ catMaybes xs

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems []   str = [] 
getItems (x:xs) str = maybeToList (lookup x (toList str)) 
                      ++ getItems xs str
  
-- Wrong codes  
--let store = (toList str)
--in  case (lookup x store) of
    --Nothing -> [] ++ getItems xs
    --Just a  -> [a] ++ getItems xs
                       
  
-- Exercise 4

--Tutorial8> theDB <- readDB
--(0.73 secs, 874,802,800 bytes)

--Tutorial8> getSample theDB
--"0026392059549"
--(0.01 secs, 138,408 bytes)
--Tutorial8> get it theDB
--Just ("XMAS DESIGNER TINS 2PC SET","EACH")
--(0.02 secs, 7,873,856 bytes)
 
-- For Exercises 6-10 check KeymapTree.hs 

--Tutorial8> theDB <- readDB
--(4.09 secs, 1,957,021,584 bytes)
--Tutorial8> getSample theDB
--"0681131435543"
--(0.12 secs, 99,849,176 bytes)
--Tutorial8> get it theDB
--Just ("Member's Mark Advanced Multi Vitamin","210 Tablets")
--(0.06 secs, 68,906,848 bytes)

-- Exercise 12
--filterLT :: Barcode -> [(Barcode, Item)] -> [(Barcode, Item)]
--filterLT code ((k, b):xs)  
    --           code <= k  = (k,b) : filterLT code xs
    --           otherwise  = filterLT code xs

-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
