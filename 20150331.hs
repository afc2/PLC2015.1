hexChar :: Char -> Int
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'B' = 11
    | ch == 'C' = 12
    | ch == 'D' = 13
    | ch == 'E' = 14
    | ch == 'F' = 15
    | otherwise     = 0

parseHex :: String -> Int
parseHex hxStr
    | length hxStr /= 0 = (hexChar(last(hxStr)))+(16*parseHex(init(hxStr)))
    | otherwise         = 0

intHex :: Int -> Char
intHex val  | val == 0 = '0'
            | val == 1 = '1'
            | val == 2 = '2'
            | val == 3 = '3'
            | val == 4 = '4'
            | val == 5 = '5'
            | val == 6 = '6'
            | val == 7 = '7'
            | val == 8 = '8'
            | val == 9 = '9'
            | val == 10 = 'A'
            | val == 11 = 'B'
            | val == 12 = 'C'
            | val == 13 = 'D'
            | val == 14 = 'E'
            | val == 15 = 'F'
            | otherwise = '0'

retornaResto :: Int -> String
retornaResto val | val < 16 = [intHex val]
                 | otherwise = [intHex (val - ( (val `div` 16) *16 ))] ++ retornaResto (val `div` 16)


somatorio :: [String] -> Int
somatorio [] = 0
somatorio h | head h == [] = somatorio (tail h) 
            | otherwise = parseHex (head h) + somatorio(tail h)

somatorioHexadecimal :: [String] -> String
somatorioHexadecimal list = reverse(retornaResto(somatorio list))


getElem :: Int -> [Char] -> Char
getElem pos list | pos == 1 = head list
                 | otherwise = getElem (pos-1) (tail list)

compara ::Int -> Int -> [Char] -> Bool
compara b e [] = False
compara b e l = (getElem b l) == (getElem e l)

palindromo :: Int -> Int -> [Char] -> Bool
palindromo b e l | (b < (length l) `div` 2) && (e > ((length l) - (length l) `div` 2)) = (compara b e l) && palindromo (b+1) (e-1) l
                 | otherwise = compara b e l

call :: [Char] -> [Char]
call [] = "NAO-PALINDROMO"
call l | palindromo 1 (length l) l = l++"  PALINDROMO"
       | otherwise = l++"  NAO-PALINDROMO"

       
call2 :: [Char] -> [Char]
call2 l = call (show (parseHex l))