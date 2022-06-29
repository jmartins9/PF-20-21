import Data.Char

--2

a = [2^x | x <- [1..10]]

b = [(x,y) | x <- [1..5] , y <- [1..5] , x+y == 6]
-- ou [(1+i,5-i) | i <- [0..4]] 

c = [[1..x] | x <- [1..5]] 

d = [replicate x 1 | x <- [1..5]]  

-- 3

digitAlpha :: String -> (String,String) 
digitAlpha [] = ([],[])
digitAlpha (x:xs) | isAlpha x = (x:l1,l2)
                  | isDigit x = (l1,x:l2) 
                  | otherwise = (l1,l2) 
            where (l1,l2) = digitAlpha xs 

-- 4

nzp :: [Int] -> (Int,Int,Int)
nzp []     = (0,0,0)
nzp (x:xs) | x < 0  = (n+1,z,p)
           | x == 0 = (n,z+1,p)
           | x > 0  = (n,z,p+1)
           where (n,z,p) = nzp xs

-- 5 

divMod' :: Integral a => a -> a -> (a,a) 
divMod' m n = (div m n, mod m n)   

myDivMod :: Integral a => a -> a -> (a,a) 
myDivMod m n | m == n    = (1,0)
             | m < n     = (0, m ) 
             | otherwise = ( d+1 , r )
           where (d ,r ) = myDivMod (m-n) n

-- 6

fromDigits :: [Int] -> Int 
fromDigits []    = 0 
fromDigits (h:t) = h*10^(length t) + fromDigits t

fromDigits' [] = 0 
fromDigits' l = aux l 0  
            where aux [] n = n
                  aux (x:xs) n = aux xs (x+10*n)

fromDigits''' l = fst $ fromDigits'' l
fromDigits'' [] = (0,0) 
fromDigits'' (x:xs) = (x*10^e+r,e+1)
       where (r,e) = fromDigits'' xs          

-- 7 

-- 8 

fib :: Int -> Int 
fib 0 = 0 
fib 1 = 1 
fib n = fib (n-1) + fib (n-2)

fibe' n = fst $ fibe n
fibe 0 = (0,0)
fibe 1 = (1,0) 
fibe n = (r+ra,r)
     where (r,ra) = fibe (n-1)     
                              


       



