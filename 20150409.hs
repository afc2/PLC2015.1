import Data.Char

sqrtList xs = map sqrt xs

posicao :: Char -> Int
posicao chr = (ord chr) - (ord 'a') + 1

posicaoAlfabeto :: String -> [Int]
posicaoAlfabeto str = maps posicao str

maps :: (t->u) -> [t] -> [u]
maps f xs = [f x | x <- xs] 

member :: Eq t => t -> [t] -> Bool
member mb xs =  foldr (||) False (map (==mb) xs)	

unionAux :: Eq t => t -> [t] -> [t]
unionAux element list
	| elem element list = list
	| otherwise = [element] ++ list

union :: Eq t => [t] -> [t] -> [t]
union l1 l2 = foldr (unionAux) [] (l1++l2)

transformStr :: String -> Int
transformStr list = foldr (+) 0 (posicaoAlfabeto list)

transform :: [String] -> [Int]
transform list = map (transformStr) list

