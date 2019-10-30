-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 4
--
-- Week 4(07-11 Oct.)

module Tutoria4 where

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:irene.vp@ed.ac.uk\">Irene Vlassi</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br><b>TA:</b> "
            , "mailto:irene.vp@ed.ac.uk\">Irene Vlassi</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Philip Wadler","wadler@inf.ed.ac.uk")
               , ("Irene Vlassi","irene.vp@ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
-- Recursion
--sameString former later = (lowerStr former) == (lowerStr later)
--  where
--  lowerStr :: String -> String
--  lowerStr [ ] = [ ]
--  lowerStr (x:xs)
--           | not (isAlpha x) = x : lowerStr xs
--           | otherwise       = toLower x : lowerStr xs

-- List comprehension
-- sameString former later = [toLower x | x <- former] == [toLower y | y <- later]

-- Map function
sameString former later = (map toLower former) == (map toLower later)



-- 2.
prefix :: String -> String -> Bool
prefix key str= sameString key (take (length key) str)

prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) &&
                         prefix substr (map toUpper str)
                           where
                             substr  =  take n str

prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
                          where substr = take n str
        
        
-- 3.
contains :: String -> String -> Bool
contains str key = or(check str key)
  where
  check :: String -> String -> [Bool]
  check [ ] _ = [False]
  check _ [ ] = [False]
  check (x:xs) key
        | toLower x == toLower (head key)  = prefix (tail key) xs:check xs key 
        | otherwise                        = check xs key
        

prop_contains :: String -> Int -> Int -> Bool
--prop_contains str a b= contains str ((str !! a):(str !! b):[ ]) 
prop_contains = undefined

-- 4.
takeUntil :: String -> String -> String
takeUntil key [ ] = [ ]
takeUntil [ ] str = str
takeUntil key (y:ys)
          | prefix (tail(key)) ys =[ ]
          | otherwise     = y: takeUntil key ys


 
dropUntil :: String -> String -> String
dropUntil key str= drop ((length (takeUntil key str)) + (length key)) str


-- 5.
split :: String -> String -> [String]
split k [ ] = [[ ]]
split "" _  = error "Wrong!"
split k str
      | contains str k= takeUntil k str : split k (dropUntil k str)
      | otherwise     = [str]

reconstruct :: String -> [String] -> String
reconstruct key str= concat (combine key str)
  where
  combine :: String -> [String] -> [String]
  combine k [ ] = [[ ]]
  combine k (x:xs) | x == last str = x:[ ]
                   | otherwise = (x++k) : combine k xs
                

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML testHTML= tail(split "<a href=\"" testHTML)
            
  
testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails inf= tail testLinks


-- 8.
link2pair :: Link -> (Name, Email)
link2pair str = (takeUntil "</a>" (dropUntil "\">" str),takeUntil "\">" (dropUntil "mailto:" str))


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html= [link2pair x | x <- takeEmails (linksFromHTML html)]

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail name str = [ x | x <- str, contains (fst x) ((head name): [head (tail name)])] 


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html nm = [ x| x <- findEmail nm (emailsFromHTML html)]


-- Optional Material

-- 13.
hasInitials :: String -> Name -> Bool
hasInitials cap str = cap == [x | x <- str, isUpper x]

-- 14.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML = undefined

emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML = undefined

-- 15.

-- If your criteria use parameters (like hasInitials), change the type signature.
myCriteria :: Name -> Bool
myCriteria = undefined

emailsByMyCriteriaFromHTML :: HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML = undefined

-- 16.
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
