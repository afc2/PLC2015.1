import Control.Monad
import Data.Char

checar :: Maybe String -> Maybe String
checar a | checarAux a = a
         | otherwise = Nothing

checarAux :: Maybe String -> Bool
checarAux (Just []) = True
checarAux (Just(a:as)) | (a == ' ' ) || (isLetter a) = checarAux (Just as)
                       | otherwise = False

quebrar :: Maybe String -> [String]
quebrar Nothing = [[]]
quebrar (Just a) =  quebrar2 a []

quebrar2 :: String -> String -> [String]
quebrar2 [] l = [l]
quebrar2 (x:xs) l | x == ' '  = (l: (quebrar2 xs []))
                  | otherwise = quebrar2 xs (l++[x])


caps :: Maybe String -> Maybe String
caps Nothing = Nothing
caps (Just e) = Just(map toUpper e) 

roda = do
	e <- getLine
	e2 <- return (checar (Just e))
	e3 <- return (caps e2)
	e4 <- return (quebrar e3) 
	mapM_ putStrLn e4