data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

e :: Exp Int
e = Mais (Const 3) (Mult (Const 4) (Const 5))  -- 3 + 4 * 5

e1 :: Exp Int
e1 = Const (-43) 


showExp :: Show a => Exp a -> String 
showExp (Const v) = show v
showExp (Simetrico e) = "- " ++ (showExp e)
showExp (Mais e d) = "(" ++ showExp e ++ " + " ++ showExp d ++ ")"
showExp (Menos e d) = "(" ++ showExp e ++ " - " ++ showExp d ++ ")"
showExp (Mult e d) = "(" ++ showExp e ++ " * " ++ showExp d ++  ")"  

instance Show a => Show (Exp a) where 
      show = showExp

calcula :: Num a => Exp a -> a
calcula (Const a) = a 
calcula (Simetrico e) = -(calcula e)
calcula (Mais e d) = calcula e + calcula d
calcula (Menos e d) = calcula e - calcula d
calcula (Mult e d) = calcula e * calcula d  

eqExp :: (Eq a, Num a) => Exp a -> Exp a -> Bool 
eqExp e1 e2 = calcula e1 == calcula e2

instance (Eq a,Num a) => Eq (Exp a) where 
      (==) = eqExp

miExp :: (Num a ,Ord a) => Exp a -> Exp a -> Bool
miExp e1 e2 = calcula e1 <= calcula e2 

instance (Ord a,Num a) => Ord (Exp a) where 
      (<=) = miExp

addExp :: Num a => Exp a -> Exp a -> Exp a 
addExp e1 e2 = Const (calcula e1 + calcula e2) 

subExp :: Num a => Exp a -> Exp a -> Exp a 
subExp e1 e2 = Const (calcula e1 - calcula e2) 

multExp :: Num a => Exp a -> Exp a -> Exp a 
multExp e1 e2 = Const (calcula e1 * calcula e2) 

absExp :: Num a => Exp a -> Exp a 
absExp e = Const (abs (calcula e))

signumExp :: Num a => Exp a -> Exp a 
signumExp e = Const (signum (calcula e)) 

fromIntegerExp :: Num a => Integer -> Exp a 
fromIntegerExp i = Const (fromInteger i)

instance  Num a => Num (Exp a) where
      (+) = addExp
      (-) = subExp
      (*) = multExp
      abs = absExp
      signum = signumExp 
      fromInteger = fromIntegerExp

      
 
