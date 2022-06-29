data ExpInt = Const Int 
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt
            deriving Show

e :: ExpInt 
e = Mais (Const 3) (Mult (Const 4) (Const 5))

e1 = ((Const 3) `Mais` (Const 5)) `Mult` (Const 5)

calcula :: ExpInt -> Int
calcula (Const a) = a 
calcula (Simetrico e) = -(calcula e)
calcula (Mais e d) = calcula e + calcula d
calcula (Menos e d) = calcula e - calcula d
calcula (Mult e d) = calcula e * calcula d  

infixa :: ExpInt -> String
infixa (Const a) = show a 
infixa (Simetrico e) = "- " ++ (infixa e)
infixa (Mais e d) = "(" ++ infixa e ++ " + " ++ infixa d ++ ")"
infixa (Menos e d) = "(" ++ infixa e ++ " - " ++ infixa d ++ ")"
infixa (Mult e d) = "(" ++ infixa e ++ " * " ++ infixa d ++  ")"  

posfixa :: ExpInt -> String
posfixa (Const a)     = show a 
posfixa (Simetrico e) = "- " ++ posfixa e  
posfixa (Mais e d)    = posfixa e ++ posfixa d ++ " + "
posfixa (Menos e d)   = posfixa e ++ posfixa d ++ " - "
posfixa (Mult e d)    = posfixa e ++ posfixa d ++ " * " 


data RTree a = R a [RTree a]
             deriving Show
        

rt :: RTree Int 
rt = R 7 [ R 3 [ R 8 []]
         , R 4 [ R 1 [R 3 []]
               , R 70 []
               , R 10 []
               , R 20 []
               ]
         , R 5 []
         ]

soma :: Num a => RTree a -> a
soma (R a l) = a + sum (map soma l)

--ou

soma1 (R a l) = a + foldr (+) 0 (map soma l)

soma2 (R a l) = a + foldr1 (+) (map soma l)

 
altura :: RTree a -> Int
altura (R a []) = 1
altura (R a l) = 1 + maximum (map altura l)

--ou 

altura1 (R a l) = 1 + foldr (\rt al -> max (altura rt) al) 0 l

prune :: Int -> RTree a -> RTree a
prune x (R a l) | x == 1    = R a []  
                | otherwise = R a (map (prune (x-1)) l)

mirror :: RTree a -> RTree a
mirror (R i l) = R i (reverse (map mirror l))

postorder :: RTree a -> [a]
postorder (R i l) = (concat (map postorder l)) ++ [i]





