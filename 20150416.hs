 import Data.List

 f :: (Ord t) => [t] -> t ->[t]
 f y k = [a|a <- y, (a<= k)]



 listPartitioner :: (Num a ,Ord a) => [a] -> ([a] ->[[a]])
 listPartitioner (x:xs) = j 
                 where j l = (tira(map (f l) (sort(x:xs)) ) []) ++ [sort(junta2 (map (f l) (sort(x:xs))) l)]

 existe :: (Eq t) => [t] -> t -> Bool
 existe [] _ = False
 existe a i | head a == i = True
            | otherwise = existe (tail a) i 

 existe1  ::(Eq t) => [t] -> [t] -> [t]
 existe1 [] _ = []
 existe1 _ [] = []
 existe1 a b |  not (existe b (head a)) = (head a : existe1  (tail a) b)
             | otherwise =existe1 (tail a) b

 junta :: [[t]] -> [t]
 junta [[]] = []
 junta (x:[]) = x
 junta (x:xs) = x ++ junta xs

 junta2  :: (Eq t) => [[t]] -> [t] -> [t]
 junta2 a b = existe1 b (nub(junta a))

 final :: (Eq t) =>[t] -> [t]
 final [] = []
 final a | existe (tail a) (head a) = final (tail a)
         | otherwise = [head a] ++ final (tail a) 


 add2 ::  (Eq t)=>[t] -> [t] -> [t]
 add2 a b = filter (f) (a++b) where f x = ((existe a x) && not( existe b x))

 differ::  (Eq t)=>[t] -> [t] -> [t] 
 differ a b = final (add2 a b)
 
 tira ::  (Eq t,Ord t)=> [[t]] -> [t] -> [[t]]
 tira [[]] _ = [[]]
 tira l [] =  [head l] ++ tira (tail l) (head l)
 tira (x:[]) a = [sort(differ x a)]
 tira (x:xs) a = [sort(differ x a)] ++ tira (xs) (x)