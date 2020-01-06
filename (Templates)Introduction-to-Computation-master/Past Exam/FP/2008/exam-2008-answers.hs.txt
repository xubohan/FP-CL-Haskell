import Test.QuickCheck
import Char

-- 1a.

f :: [Int] -> Int
f xs  =  maximum [ x | x <- 0 : xs, 0 <= x, x <= 100 ]

prop_f  =
    f [40,50,60]       	==  60  &&
    f [40,30,50,70,60] 	==  70  &&
    f [-10,20,80,110]  	==  80  &&
    f [-10,110]        	==   0  &&
    f []               	==   0

-- 1b.

g :: [Int] -> Int
g []                           =  0
g (x:xs) | 0 <= x && x <= 100  =  x `max` g xs
         | otherwise           =  g xs

prop_g  =
    g [40,50,60]       	==  60  &&
    g [40,30,50,70,60] 	==  70  &&
    g [-10,20,80,110]  	==  80  &&
    g [-10,110]        	==   0  &&
    g []               	==   0

-- 1c.

h :: [Int] -> Int
h xs  =  foldr max 0 (filter p xs)
  where
  p x  =  0 <= x && x <= 100

prop_h  =
    h [40,50,60]       	==  60  &&
    h [40,30,50,70,60] 	==  70  &&
    h [-10,20,80,110]  	==  80  &&
    h [-10,110]        	==   0  &&
    h []               	==   0

prop_fg xs  =  f xs == g xs
prop_gh xs  =  g xs == h xs

ok1 =
  quickCheck prop_f  >>      
  quickCheck prop_g  >>      
  quickCheck prop_h  >>      
  quickCheck prop_fg >>   
  quickCheck prop_gh   

-- 2a.

p :: Int -> Int -> [Int]

p x y | x < y      =  [x, x+1 .. y-1]
      | otherwise  =  [x, x-1 .. y+1]

prop_p  =
  p 3 7  ==  [3,4,5,6]  &&
  p 7 3  ==  [7,6,5,4]  &&
  p 7 7  ==  []

-- 2b.

r :: [Int] -> [Int]
r zs | not (null zs)  =
  concat [ p x y | (x,y) <- zip zs (tail zs) ]

prop_r  =
  r [3,7,4,8]      ==  [3,4,5,6,7,6,5,4,5,6,7]  &&
  r [1,3,5,3,5,7]  ==  [1,2,3,4,5,4,3,4,5,6] &&
  r [3,7,7,7,3]    ==  [3,4,5,6,7,6,5,4] &&
  r [7,7,7]        ==  []

-- 2c.

s :: [Int] -> [Int]
s [x]       =  []
s (x:y:zs)  =  p x y ++ s (y:zs)

prop_s  =
  s [3,7,4,8]      ==  [3,4,5,6,7,6,5,4,5,6,7]  &&
  s [1,3,5,3,5,7]  ==  [1,2,3,4,5,4,3,4,5,6] &&
  s [3,7,7,7,3]    ==  [3,4,5,6,7,6,5,4] &&
  s [7,7,7]        ==  []

prop_rs xs  =  
  not (null xs)  ==> 
    take 1000 (r xs) == take 1000 (s xs)

ok2 =
  quickCheck prop_p  >>     
  quickCheck prop_r  >>       
  quickCheck prop_s  >>     
  quickCheck prop_rs    

-- 3a.

t :: [Int] -> Int
t xs  =  sum [ 1 | (x,y) <- zip ([1]++xs) (xs++[1]), ok x y ]
  where
  ok x y  =  (x==0 && y/=0)

prop_t =
  t []                 ==  0  &&
  t [0,0,0]            ==  1  &&
  t [0,0,1,1,0,0]      ==  2  &&
  t [1,0,0,0,0,1]      ==  1  &&
  t [1,1,0,0,2,2,0,0]  ==  2


-- 3b.

u :: [Int] -> Int
u xs  =  nonzero xs
  where
  nonzero (0:xs)           =  1 + zero xs
  nonzero (x:xs) | x /= 0  =  nonzero xs
  nonzero []               =  0
  zero (x:xs) | x /= 0     =  nonzero xs
  zero (0:xs)              =  zero xs
  zero []                  =  0

prop_u =
  u []                 ==  0  &&
  u [0,0,0]            ==  1  &&
  u [0,0,1,1,0,0]      ==  2  &&
  u [1,0,0,0,0,1]      ==  1  &&
  u [1,1,0,0,2,2,0,0]  ==  2

prop_tu xs  =  t xs == u xs

ok3 =
  quickCheck prop_t  >>
  quickCheck prop_u  >>
  quickCheck prop_tu

ok_all =
  ok1 >>
  ok2 >>
  ok3
