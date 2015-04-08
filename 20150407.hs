data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = r * r * pi
area (Rectangle l1 l2) = l1 * l2

data Dias = Domingo | Segunda Int [String]| Terca Int [String] | Quarta Int [String] | Quinta Int [String] | Sexta Int [String] | Sabado

isWeekDay :: Dias -> Bool
isWeekDay (Domingo) = True
isWeekDay (Sabado) = True
isWeekDay _ = False

isPLC :: Dias -> Bool
isPLC (Domingo) = False
isPLC (Sabado) = False
isPLC (Segunda h ls) = checkPLC ls
isPLC (Terca h ls) = checkPLC ls
isPLC (Quarta h ls) = checkPLC ls
isPLC (Quinta h ls) = checkPLC ls
isPLC (Sexta h ls) = checkPLC ls

checkPLC :: [String] -> Bool
checkPLC (x : xs)
		| x == "PLC" = True
		| xs == [] = False
		| otherwise = checkPLC xs

data Tree t = NilT | Node t (Tree t) (Tree t)
	deriving (Eq, Show)

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr
	deriving (Show)

showExpr :: Expr -> String
showExpr (Lit a) = show(a)
showExpr (Add a b) = showExpr(a) ++ "+" ++ showExpr(b)
showExpr (Sub a b) = showExpr(a) ++ "-" ++ showExpr(b)

data List t = Nil | Cons t (List t)
	deriving (Show)

toList :: List t -> [t]
toList (Nil) = []
toList (Cons a ls) = a:toList(ls)

fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = Cons a (fromList as)

depth :: Tree t -> Int
depth (NilT) = 0
depth (Node a b c) = 1 + (max (depth b) (depth c)) 