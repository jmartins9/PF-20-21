module Ficha1 where 

import Data.Char 

--1 a 

perimetro :: Float -> Float 
perimetro r = if r < 0 then error "O raio é um valor positivo"
              else 3.14 * 2 * r 

--1 b 

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

--1 c 
 
primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)  

--1 d

multiplo :: (Integral a) => a -> a -> Bool 
multiplo x y = mod x y == 0 

--1 e 

truncaImpar :: [a] -> [a]
truncaImpar l | mod (length l) 2 == 0 = l 
              | otherwise             = tail l

--1 f 

max2 :: Int -> Int -> Int 
max2 x y | x >= y    = x 
         | otherwise = y  

--1 g 

max3 :: Int -> Int -> Int -> Int 
max3 x y z = max2 (max2 x y) z 

--2 a 

nRaizes :: Float -> Float -> Float -> Float 
nRaizes a b c | a == 0 = error "Polinómio não é do segundo grau"
              | (b^2 - 4*a*c) < 0  = 0
              | (b^2 - 4*a*c) == 0 = 1
              | otherwise          = 2 

-- 2 b 

raizes :: Float -> Float -> Float -> [Float]
raizes a b c | nRaizes a b c == 1 = [(-b)/(2*a)] 
             | nRaizes a b c == 2 = [((-b) - sqrt (b^2-4*a*c))/(2*a),((-b) + sqrt (b^2-4*a*c))/(2*a)]
             | otherwise          = []

-- 3 a 

type Hora = (Int,Int)

testaHora :: Hora -> Bool 
testaHora (h,m) = h>=0 && h<24 && m>=0 && m<60 

-- 3 b 

comparaHora :: Hora -> Hora -> Bool 
comparaHora (h1,m1) (h2,m2) | h1>h2          = True 
                            | h1<h2          = False
                            | m1>m2          = True
                            | otherwise      = False  
                                           
-- 3 c 

horasParaMinutos :: Hora -> Int 
horasParaMinutos (h,m) = h*60 + m 

-- 3 d 

minutosParaHoras :: Int -> Hora
minutosParaHoras x = (div x 60, mod x 60) 

-- 3 e 

diferencaHoras :: Hora -> Hora -> Int 
diferencaHoras (h1,m1) (h2,m2) = abs ((h1-h2) * 60) + abs (m1-m2) 

-- 3 f

adicionaMinutos :: Hora -> Int -> Hora
adicionaMinutos h x = minutosParaHoras (horasParaMinutos h + x) 

-- 4 

data Hora' = H Int Int 
          deriving (Show,Eq)

testaHora' :: Hora' -> Bool 
testaHora' (H h m) = testaHora (h,m)

-- quando tiver tempo acabar o 4

-- 5 a 

data Semaforo = Verde | Amarelo | Vermelho 
              deriving (Show,Eq) 

next :: Semaforo -> Semaforo 
next Verde    = Amarelo  
next Amarelo  = Vermelho 
next Vermelho = Verde

-- 5 b 

stop :: Semaforo -> Bool 
stop Vermelho = True 
stop Amarelo  = True
stop Verde    = False 

-- 5 c 

safe :: Semaforo -> Semaforo -> Bool 
safe Vermelho _ = True
safe _ Vermelho = True 
safe _ _        = False 

-- 6 

data Ponto = Cartesiano Double Double | Polar Double Double 
           deriving (Show,Eq) 

posx :: Ponto -> Double 
posx (Cartesiano x y) = abs x 
posx (Polar x y)      = abs ((cos y) * x)

posy :: Ponto -> Double 
posy (Cartesiano x y) = abs y 
posy (Polar x r)      = abs ((sin r) * x)

raio :: Ponto -> Double 
raio (Polar x r)      = x 
raio (Cartesiano x y) = sqrt (x^2 + y^2)

angulo :: Ponto -> Double  
angulo (Polar x r)      = r 
angulo (Cartesiano x y) | x>0 && y>0 = atan (y/x) * 180/pi
                        | x<0 && y>0 = 180 + atan (y/x) * 180/ pi
                        | x<0 && y<0 = (atan (y/x)) * 180/pi + 180
                        | x>0 && y<0 = atan (y/x) * 180/pi + 36

dist' :: Ponto -> Ponto -> Double 
dist' (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)
dist' (Polar x1 a1) (Polar x2 a2)           = sqrt ((cos a1 * x1 - cos a2 * x2)^2 + (sin a1 * x1 - sin a2 * x2)^2)
dist' (Polar x1 a1) (Cartesiano x y)        = sqrt ((cos a1 * x1 - x)^2 + (sin a1 * x1 - y)^2)
dist' (Cartesiano x y) (Polar x1 a1)        = sqrt ((x - cos a1 * x1)^2 + (y - sin a1 * x1)^2)

-- 7 

data Figura = Circulo Ponto Double 
            | Rectangulo Ponto Ponto 
            | Triangulo Ponto Ponto Ponto 
            deriving (Show,Eq) 

poligono :: Figura -> Bool 
poligono (Circulo _ r)                                                        = r>0 
poligono (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2))                   = (x1 /= x2) && (y1 /= y2)
poligono (Triangulo (Cartesiano x1 y1) (Cartesiano x2 y2) (Cartesiano x3 y3)) = (Cartesiano x1 y1) /= (Cartesiano x2 y2) && (Cartesiano x2 y2) /= (Cartesiano x3 y3) && (Cartesiano x1 y1) /= (Cartesiano x3 y3) 

vertices :: Figura -> [Ponto] 
vertices (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = [(Cartesiano x1 y1),(Cartesiano x2 y1),(Cartesiano x2 y2),(Cartesiano x1 y2)]
vertices (Triangulo (Cartesiano x1 y1) (Cartesiano x2 y2) (Cartesiano x3 y3)) = [(Cartesiano x1 y1),(Cartesiano x2 y2),(Cartesiano x3 y3)] 

area :: Figura -> Double 
area (Triangulo p1 p2 p3) = 
    let a = dist p1 p2
        b = dist p2 p3 
        c = dist p3 p1 
        s = (a+b+c) / 2 
    in sqrt (s*(s-a)*(s-b)*(s-c)  

{-
-- 8 

isLower' :: Char -> Bool 
isLower' c = (ord c)>97 && (ord c) <122

isDigit' :: Char -> Bool 
isDigit' c = (ord c) >= 0 && (ord c) <= 9

isAlpha' ::  Char -> Bool 
isAlpha' c = ((ord c) >= 65 && (ord c) <= 90) || isLower' c 

toUpper' :: Char -> Char 
toUpper' c = chr ((ord c) - 32)

intToDigit' :: Int -> Char
intToDigit' c = chr (c + 48)

digitToInt' :: Char -> Int
digitToInt' c = (ord c) - 48      
  -} 