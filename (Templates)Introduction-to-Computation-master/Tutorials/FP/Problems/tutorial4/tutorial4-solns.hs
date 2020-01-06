-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 4
--
-- Week 4(07-11 Oct.)
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

module Tutorial4 where

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
           ++ "<b>Lecturer:</b> <a href=\"mailto:walder@inf.ed.ac.uk\">Philip Walder</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:irene.vp@ed.ac.uk\">Irene Vlassi</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:walder@inf.ed.ac.uk\">Philip Walder</a><br><b>TA:</b> "
            , "mailto:irene.vp@ed.ac.uk\">Irene Vlassi</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Walder Philip","walder@inf.ed.ac.uk")
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
sameString str1 str2 = map toUpper str1 == map toUpper str2

-- Note that the following fails on input '\181'.
prop_con a = toLower a == toLower (toUpper a)
-- 2.
prefix :: String -> String -> Bool
prefix substr str = substr `sameString` take (length substr) str

-- prefix' substr str = map toUpper substr `isPrefixOf` map toUpper str

-- prop_prefix_pos is satisfied by a function that always returns True, for example.
prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n = prefix substr (map toLower str) &&
                          prefix substr (map toUpper str)
  where
    substr = take n str
prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
  where substr = take n str

-- 3.
contains :: String -> String -> Bool
contains "" "" = True
contains str substr = or [ prefix substr (drop i str) | i <- [0..length str - 1] ]

-- We would get much faster solution if we used the "tails" function in Data.List. 
-- Which can be defined as
-- tails :: [a] -> [[a]]
-- tails []     = [[]]
-- tails (x:xs) = (x:xs):tails xs

-- We then write
-- contains str substr = or [ prefix substr s | s <- tails str ] 

-- Compare this solution when str is a huge string such as (replicate 10000 'a')

-- Or we can use the following recursive solution.
-- contains :: String -> String -> Bool
-- contains [] []         = True
-- contains [] _          = False
-- contains (c:cs) substr = prefix substr (c:cs) || contains cs substr

prop_contains :: String -> Int -> Int -> Bool
prop_contains str n m = (map toUpper str) `contains` substr &&
                        (map toLower str) `contains` substr
                              where
                                substr = take n (drop m str)

-- 4.
takeUntil :: String -> String -> String
takeUntil substr [] = ""
takeUntil substr (c:cs) | prefix substr (c:cs) = ""
                        | otherwise = c : takeUntil substr cs

dropUntil :: String -> String -> String
dropUntil substr [] = ""
dropUntil substr str | prefix substr str = drop (length substr) str
                     | otherwise = dropUntil substr (tail str)

-- A List comprehension version
-- dropUntil :: String -> String -> String
-- dropUntil substr str = case [ s | s <- tails str, prefix substr s ] of
--     []  -> ""
--     s:_ -> drop (length substr) s

-- 5.
split :: String -> String -> [String]
split "" str  = error "Can't split on an empty string"
split sep str  
    | str `contains` sep = takeUntil sep str : split sep (dropUntil sep str)
    | otherwise        = [str]

reconstruct :: String -> [String] -> String
reconstruct _ []           = []
reconstruct _ [str]        = str
reconstruct sep (str:strs) = str ++ sep ++ reconstruct sep strs

-- Alternative using foldr1:
--
-- reconstruct sep = foldr1 f
--    where
--      f xs ys = xs ++ sep ++ ys
--
-- Alternative using concat:
-- 
-- reconstruct sep (str:strs) = str ++ concat (map (sep ++) strs)
-- 
-- Alternative using intersperse:
--
-- reconstruct sep strs | not (null strs) = concat (intersperse sep strs)

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML doc = tail (split "<a href=\"" doc)

testLinksFromHTML :: Bool
testLinksFromHTML = linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails links = [link | link <- links, prefix "mailto:" link]

-- Alternative solution: 
--
-- takeEmails links = filter (prefix "mailto:") links


-- 8.
link2pair :: Link -> (Name, Email)
link2pair link | link `contains` "mailto:" = (name, email)
               | otherwise = error "link2pair: not a mail adress"
    where email = takeUntil "\">"  (dropUntil "mailto:" link)
          name  = takeUntil "</a>" (dropUntil "\">" link)


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html = nub [link2pair link | link <- takeEmails (linksFromHTML html)]

-- Alternative solution:
--
-- emailsFromHTML = nub . map link2pair . takeEmails . linksFromHTML

testEmailsFromHTML :: Bool
testEmailsFromHTML = emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail name addrs = [(n, e) | (n, e) <- addrs, n `contains` name]


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html name = findEmail name (emailsFromHTML html)


-- Optional Material

--13.
hasInitials :: String -> Name -> Bool
hasInitials [] [] = True
hasInitials [] _ = False
hasInitials _ [] = False
hasInitials (x:xs) (y:ys) | (toLower x) == (toLower y) = hasInitials xs ys' 
                          | otherwise = False
  where ys' = dropUntil " " ys
        
-- 14.
findEmailByMatch :: (Name -> Bool) -> [(Name, Email)] -> [(Name, Email)]
findEmailByMatch match addrs = [(n,e)|(n,e) <- addrs, match n]

emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name,Email)]
emailsByMatchFromHTML match html = findEmailByMatch match (emailsFromHTML html)

emailsByInitialsFromHTML :: String -> HTML -> [(Name,Email)]
emailsByInitialsFromHTML initials = emailsByMatchFromHTML (hasInitials initials)

-- 15.
-- We match names with initials containing the letters of the first argument, in
-- that order but not necessarily consecutively.

myCriteria :: String -> Name -> Bool
myCriteria [] _ = True
myCriteria _ [] = False
myCriteria (x:xs) (y:ys) | (toLower x) == (toLower y) = myCriteria xs ys'
                         | otherwise = myCriteria (x:xs) ys'
  where ys' = dropUntil " " ys

emailsByMyCriteriaFromHTML :: String -> HTML -> [(Name,Email)]
emailsByMyCriteriaFromHTML initials = emailsByMatchFromHTML (myCriteria initials)

-- 16.
--ppAddrBook :: [(Name, Email)] -> String
--ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]

ppAddrBook addr = unlines [ take (max + 2) (formatName name ++ repeat ' ') ++ email 
                          | (name,email) <- addr ] 
  where max = maximum (map (length . fst) addr)

formatName name  
    | name `contains` "," = name
    | otherwise = dropUntil " " name ++ ", " ++ takeUntil " " name
