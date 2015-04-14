data Tree t = 	NilT | Node t (Tree t) (Tree t)
				deriving (Eq, Show)

test :: Tree Int
test = (Node 5 (Node 7 (Node 15 NilT (Node 6 (Node 13 (Node 5 NilT NilT) NilT) NilT)) (Node 2 NilT NilT)) (Node 10 NilT NilT))

filterS :: (t -> Bool) -> Tree t -> Tree t
filterS f NilT = NilT
filterS f (Node n l r)
 | f n = (Node n (filterS f l) (filterS f r))
 | otherwise = NilT

subTree :: (t -> Bool) -> Tree t -> [Tree t]
subTree f NilT = []
subTree f (Node n l r)
 | f n = (subTree f l) ++ (subTree f r)
 | otherwise = (filterTree f l) ++ (filterTree f r)

filterTree :: (t -> Bool) -> Tree t -> [Tree t]
filterTree f NilT = []
filterTree f (Node n l r)
 | f n = [filterS f (Node n l r)] ++ (subTree f l) ++ (subTree f r)
 | otherwise = (filterTree f l) ++ (filterTree f r)

map2 :: (t -> t) -> [(t->t)] -> [(t->t)]
map2 f [] = []
map2 f (a:as) = funcOFunc : map2 f as
      where funcOFunc y = f (a y)
 
compose ::  (t->t) -> [(t->t)] -> [(t->t)]
compose _ []  = []
compose f x= map2 f x