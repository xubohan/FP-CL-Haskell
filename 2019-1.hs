import Data.Char
import Data.List
import Test.QuickCheck

q :: [String] -> String
q [] = "zzz"
q x  = minimum [ if length a < 4 && and[isLower c | c <- a] then a else "zzz" | a <- x]

p :: [String] -> String
p x = comp (f x)
  where
    f [] = []
    f (x:xs)
        | length x < 4 
          && det x      = x : f xs
        | otherwise     = f xs   

    det [] = True
    det (x:xs) = isLower x && det xs 

    db x y 
     | x > y = y
     | x < y = x
     | x == y = x

    comp [] = "zzz"
    comp [x] = x
    comp (x:y:z) 
        | x < y = db x (comp z)
        | x > y = db y (comp z)
        | x == y = db x (comp z)
  
r :: [String] -> String
r x = foldr (db) "zzz" (filter (\a -> length a < 4 && and (map (isLower) a)) x)
    where
        db x y 
            | x > y = y
            | x < y = x
            | x == y = x

prop1 :: [String] -> Bool    
prop1 x = q x == p x && r x == p x       
        

--abc def ||| abf
--init 返回不带最后一个Char的值
--last 返回最后一个Char的值
f :: String -> String -> String
f x y = init x ++ [last y]

f' :: [String] -> [String]
f' (x:xs) = [f a b | (a,b) <- zip (x:xs) (xs++[x])] 

f'' :: [String] -> [String]
f'' x = fa (zip' x (tail x ++ [head x]))
    where
     fa :: [(String,String)] -> [String]
     fa [] = []
     fa ((x,y):z) = f x y : fa z   
    
     zip' :: [String] -> [String] -> [(String,String)]
     zip' x [] = []
     zip' [] y = []
     zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

prop2 :: [String] -> Bool
prop2 x = f' x == f'' x





