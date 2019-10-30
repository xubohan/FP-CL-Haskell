
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:s | s <- powerset xs] ++ powerset xs

div2 :: [Int]
div2 = take 100 [x | x <- [1..], x `mod` 2 == 0]
div3 :: [Int]
div3 = take 100 [x | x <- [1..], x `mod` 3 == 0]

union :: [Int] -> [Int] -> [Int]
union setA setB = [x | x <- [1..], x `elem` setA || x `elem` setB]

