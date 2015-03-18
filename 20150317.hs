vendas :: Int->Int
vendas x = x * x

func ::  Int->Int->Int
func s n | n == 0 && vendas n == s = 1
         | n == 0  = 0
         | vendas n == s = 1 + func s (n - 1) 
         | otherwise = func s (n - 1)