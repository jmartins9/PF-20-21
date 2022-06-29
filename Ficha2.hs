{-Ficha 2-}

                         
dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = 2 * x : dobros xs 

numOcorre :: Char -> String -> Int 
numOcorre c []    = 0 
numOcorre c (h:t) = if c == h then 1 + numOcorre c t
                    else numOcorre c t 
-- ou --
numOcorre' _ [] = 0
numOcorre' c (h:t) | c == h    = 1 + numOcorre' c t
                   | otherwise numOcorre' c t 

positivos :: [Int] -> Bool
positivos [h]   = h > 0 
positivos (h:t) | h > 0 = positivos t
                | otherwise = False 

soPos :: [Int] -> [Int] 
soPos []     = []
soPos (x:xs) | x > 0 = x : soPos xs 
	         | otherwise soPos xs

somaNeg :: [Int] -> Int 
somaNeg []    = 0
somaNeg (h:t) | h < 0 = h + somaNeg t
              | otherwise somaNeg t

tresUlt :: [a] -> [a] 
tresUlt (a:b:c:[]) = [a,b,c]
tresUlt (a:b:c:t)  = tresUlt (b:c:t)    
tresUlt l          = l

segundos :: [(a,b)] -> [b]                                  
segundos []        = []  
segundos ((_,b):t) = b : segundos t   
-- ou
segundos' []     = []
segundos' (x:xs) = snd x : segundos' xs                        

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c) 
sumTriplos [x] = x
sumTriplos ((x,y,z):t) = (x+xs , y+ys , z+zs) 
      where (xs,ys,zs) = sumTriplos t

type Polinomio = [Monomio]

type Monomio = (Float,Int) 

p :: Polinomio 
p = [(2,3), (3,4) , (5,3) , (4,5)]

grauMonomio :: Monomio -> Int
grauMonomio m = snd m

conta :: Int -> Polinomio -> Int 
conta _ [] = 0 
conta g (m:ms) = if grauMonomio m==g then 1 + conta g ms 
                 else conta g ms 

grau :: Polinomio -> Int 
grau []         = 0    
grau ((c,e):ms) | g > e = g 
                | otherwise = e 
        where g = grau ms 

selgrau :: Int -> Polinomio -> Polinomio 
selgrau _ [] = [] 
selgrau g ((c,e):ms) | g == e    = (c,e): selgrau g ms
                     | otherwise = selgrau g ms 

deriv :: Polinomio -> Polinomio 
deriv []         = []
deriv ((_,0):ms) = deriv ms   
deriv ((c,e):ms) = ( c* (fromIntegral e) , e-1 ) : deriv ms 
      

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0  
calcula x ((c,e): ms) = c * x ^ e + calcula x ms  







 