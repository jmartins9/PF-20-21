import Data.Char
import Data.List
import Data.Either 
import Data.Maybe

-- 1

myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo x y | x > y     = []
                 | x == y    = [x]
                 | otherwise = (x : myEnumFromTo (x+1) y)

-- 2

myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo x y z | x > z     = []
                       | x == z    = [x,x..] 
                       | otherwise = x : myEnumFromThenTo (x+a) (y+a) z 
                                   where a=y-x 
        
-- 3  

(+++) :: [a] -> [a] -> [a]
(+++) [] l      = l
(+++) (x:xs) l = x : (+++) xs l 

-- 4 

(!!!) :: [a] -> Int -> a 
(!!!) (x:xs) a | a == 0    = x 
               | otherwise = (!!!) xs (a-1)  

-- 5 

myReverse :: [a] -> [a] 
myReverse [] = [] 
myReverse (x:xs) = myReverse xs ++ [x] 

-- 6 

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake a (x:xs) | a <= 0            = [] 
                | length (x:xs) < a = (x:xs)             
                | otherwise         = (x : myTake (a-1) xs) 

-- 7 

myDrop :: Int -> [a] -> [a]
myDrop _ [] = [] 
myDrop a (x:xs) | a <= 0            = (x:xs)
                | length (x:xs) < a = []
                | otherwise         = myDrop (a-1) xs  

-- 8 

myZip :: [a] -> [b] -> [(a,b)] 
myZip _ []          = [] 
myZip [] _          = []
myZip (x:xs) (y:ys) = (x,y): myZip xs ys    

-- 9 

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem a (x:xs) | a == x    = True 
                | otherwise = myElem a xs  

-- 10 

myReplicate :: Int -> a -> [a] 
myReplicate x y | x <= 0    = []
                | otherwise = y:myReplicate (x-1) y 

-- 11 

myIntersperse :: a -> [a] -> [a] 
myIntersperse _ [] = []
myIntersperse a (x:xs) = x:a:myIntersperse a xs 

-- 12 

myGroup ::  Eq a => [a] -> [[a]] 
myGroup []     = []
myGroup [a]    = [[a]] 
myGroup (x:xs) | x == head b = (x:b) : c
               | otherwise   = [x]: (b:c)
               where (b:c) = myGroup xs 
                      
-- 13                   

myConcat :: [[a]] -> [a] 
myConcat [] = []
myConcat [[]] = [] 
myConcat (x:xs) | length x == 0 = error "Os elementos não podem uma ser lista vazia" 
                | otherwise     = x ++ myConcat xs 

-- 14 

myInits :: [a] -> [[a]] 
myInits []  = [[]]  
myInits l   = myInits (b) ++ [l]
            where b = init l 

-- 15 

myTails :: [a] -> [[a]] 
myTails []  = [[]]
myTails l   = [l] ++ myTails (tail l) 

-- 16

myIsPrefixOf :: Eq a => [a] -> [a] -> Bool
myIsPrefixOf [] _  = True
myIsPrefixOf l1@(a:b) l2@(h:t) 
          | length l1 > length l2 = False
          | a == h                = myIsPrefixOf b t
          | otherwise             = False 

-- 17

myIsSuffixOf :: Eq a => [a] -> [a] -> Bool 
myIsSuffixOf [] _ = True  
myIsSuffixOf l1 l2 
          | length l1 > length l2 = False 
          | last l1 == last l2    = myIsSuffixOf (init l1) (init l2) 
          | otherwise             = False 

-- 18

myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool 
myIsSubsequenceOf [] _ = True 
myIsSubsequenceOf _ [] = False
myIsSubsequenceOf (a:b) (h:t)  
               | a /= h    = myIsSubsequenceOf (a:b) t  
               | otherwise = myIsSubsequenceOf b t 

-- 19

myElemIndices :: Eq a => a -> [a] -> [Int]
myElemIndices _ []    = [] 
myElemIndices x l | x == last l = myElemIndices x (init l) ++ [length l - 1]   
                  | otherwise   = myElemIndices x (init l)

-- 20

myNub :: Eq a => [a] -> [a]  
myNub []  = []
myNub l = f [] l
f l1 [] = l1
f l1 (h:t) | elem h l1 = f l1 t
           | otherwise = f (l1 ++ [h]) t 

-- 21

myDelete :: Eq a => a -> [a] -> [a] 
myDelete _ [] = []
myDelete a l | elem a l == False = l
myDelete a (x:xs) | a == x = xs
                  | otherwise = x : myDelete a xs   

-- 22 

(\\\) :: Eq a => [a] -> [a] -> [a] 
(\\\) [] _ = []  
(\\\) l [] = l 
(\\\) l (h:t) | elem h l  = (\\\) (aux h l) t 
              | otherwise = (\\\) l t  

aux :: Eq a => a -> [a] -> [a]
aux h l | h == head l = tail l 
        | otherwise   = (head l) : aux h (tail l)

-- 23 

myUnion :: Eq a => [a] -> [a] -> [a] 
myUnion [] l = l 
myUnion l [] = l
myUnion l (h:t) | elem h l  = myUnion l t 
                | otherwise = myUnion (l ++ [h]) t

-- 24 

myIntersect :: Eq a => [a] -> [a] -> [a] 
myIntersect l [] = l 
myIntersect [] _ = [] 
myIntersect (x:xs) l | elem x l  = x : myIntersect xs l 
                     | otherwise = myIntersect xs l  

-- 25 

myInsert :: Ord a => a -> [a] -> [a] 
myInsert a [] = [a]
myInsert a (h:t) | a <= h    = (a:h:t)
                 | otherwise = h : myInsert a t 

-- 26 

myUnwords :: [String] -> String  
myUnwords [] = " " 
myUnwords [x] = x
myUnwords (x:xs) = x ++ " " ++ myUnwords xs 

-- 27 

myUnlines :: [String] -> String 
myUnlines [] = " "
myUnlines [x] = x ++ "\n" 
myUnlines (x:xs) = x ++ "\n" ++ myUnlines xs 

-- 28 

pMaior :: Ord a => [a] -> Int 
pMaior [] = error "Exceção- lista vazia" 
pMaior (x:xs) | x == aux1 (x:xs) = 0 
              | otherwise       = 1 + pMaior xs  

aux1 :: Ord a => [a] -> a 
aux1 [] = error "Exceção- lista vazia"
aux1 [x] = x
aux1 (x:xs) = max x (aux1 xs) 

-- 29

temRepetidos :: Eq a => [a] -> Bool 
temRepetidos [] = False 
temRepetidos (x:xs) | elem x xs = True 
                    | otherwise = temRepetidos xs

-- 30 

algarismos :: [Char] -> [Char] 
algarismos [] = []
algarismos (x:xs) 
           | isDigit x = x : algarismos xs 
           | otherwise = algarismos xs  

-- 31 

posImpares :: [a] -> [a] 
posImpares [] = []
posImpares [x] = [] 
posImpares (x:y:xs) = y : posImpares xs 

-- 32 

posPares :: [a] -> [a] 
posPares [] = []
posPares [x] = [x] 
posPares (x:y:xs) = x : posPares xs

-- 33 

isSorted :: Ord a => [a] -> Bool 
isSorted [] = True 
isSorted [x] = True
isSorted (x:y:xs) | x <= y     = isSorted (y:xs) 
                  | otherwise = False 

-- 34 

iSort :: Ord a => [a] -> [a] 
iSort [] = []
iSort [x] = [x] 
iSort (x:xs) = insert x (iSort xs) 
                
-- 35 

menor :: String -> String -> Bool 
menor [] _ = True
menor _ [] = False 
menor (x:xs) (y:ys) | (ord x) < (ord y) = True 
                    | (ord x) > (ord y) = False 
                    | otherwise         = menor xs ys  

-- 36 

elemMSet :: Eq a => a -> [(a,Int)] -> Bool 
elemMSet _ [] = False 
elemMSet a ((x,y):xs) | a == x    = True 
                      | otherwise = elemMSet a xs 

-- 37 

lengthMSet :: [(a,Int)] -> Int 
lengthMSet [] = 0 
lengthMSet ((x,y):xs) = y + (lengthMSet xs)

-- 38 

converteMSet :: [(a,Int)] -> [a] 
converteMSet [] = [] 
converteMSet ((x,y):xs) 
             | y == 0    = converteMSet xs 
             | otherwise = x : converteMSet ((x,y-1):xs)

-- 39

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
insereMSet x [] = [(x,1)] 
insereMSet a ((x,y):xs) | a == x    = (x,y+1) : xs 
                        | otherwise = (x,y) : insereMSet a xs     

-- 40 
 
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet a ((x,y):xs) | a == x  = (x,y-1) : xs 
                        | otherwise = (x,y) : removeMSet a xs 

-- 41 

constroiMSet :: Ord a => [a] -> [(a,Int)] 
constroiMSet [] = []
constroiMSet (x:xs) = conta 1 (x:xs) 
            where conta n [] = []
                  conta n (x:[]) = [(x,n)]
                  conta n (x:y:xs) | x == y    = conta (n+1) (y:xs)
                                   | otherwise = (x,n) : conta 1 (y:xs)   

-- 42 

myPartitionEithers :: [Either a b] -> ([a],[b]) 
myPartitionEithers [] = ([],[])
myPartitionEithers (Left a:xs) = ((a:l1),l2) 
                   where (l1,l2) = myPartitionEithers xs 
myPartitionEithers (Right b:xs) = (l1,(b:l2))
                   where (l1,l2) = myPartitionEithers xs

-- 43

myCatMaybes :: [Maybe a] -> [a] 
myCatMaybes [] = []
myCatMaybes ((Just x):xs) = x : myCatMaybes xs 
myCatMaybes ((Nothing):xs) = myCatMaybes xs 

-- 44 

data Movimento = Norte | Sul | Este | Oeste 
               deriving Show 

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (a,b) [] = (a,b) 
posicao (a,b) (Norte:xs) = posicao (a,b+1) xs
posicao (a,b) (Sul:xs)   = posicao (a,b-1) xs
posicao (a,b) (Este:xs)  = posicao (a+1,b) xs 
posicao (a,b) (Oeste:xs) = posicao (a-1,b) xs

-- 45 

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (a,b) (c,d) | a == c && b == d = []
                    | b < d = Norte : caminho (a,b) (c,d-1)
                    | b > d = Sul : caminho (a,b) (c,d+1)                      
                    | a < c = Este : caminho (a,b) (c-1,d)
                    | a > c = Oeste : caminho (a,b) (c+1,d)

-- 46 

vertical :: [Movimento] -> Bool 
vertical [] = True 
vertical (Norte:xs) = vertical xs
vertical (Sul:xs) = vertical xs
vertical (x:xs) = False

-- 47 

data Posicao = Pos Int Int 
               deriving Show 

maisCentral :: [Posicao] -> Posicao 
maisCentral (Pos x y : []) = (Pos x y) 
maisCentral ((Pos x1 y1) : (Pos x2 y2) : xs) | distOrigem (Pos x1 y1) <= distOrigem (Pos x2 y2) = maisCentral ((Pos x1 y1) : xs) 
                                             | otherwise                                        = maisCentral ((Pos x2 y2) : xs)
                                             where distOrigem (Pos x y) = sqrt(fromIntegral(x^2+y^2)) 

-- 48 

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x y) ((Pos x1 y1):xs) | abs (x-x1) == 1 && y == y1 = (Pos x1 y1):(vizinhos (Pos x y) xs) 
                                    | abs (y-y1) == 1 && x == x1 = (Pos x1 y1):(vizinhos (Pos x y) xs)
                                    | otherwise = vizinhos (Pos x y) xs

-- 49 

mesmaOrdenada :: [Posicao] -> Bool 
mesmaOrdenada ((Pos x y):[]) = True
mesmaOrdenada ((Pos x1 y1):(Pos x2 y2):xs) | y1 == y2  = mesmaOrdenada ((Pos x2 y2):xs)
                                           | otherwise = False 

-- 50 

data Semaforo = Verde | Amarelo | Vermelho 
                deriving Show 

interseccaoOK :: [Semaforo] -> Bool 
interseccaoOK [] = False 
interseccaoOK l = contaVerdes l <= 1
                where contaVerdes [Verde] = 1
                      contaVerdes [Amarelo] = 1
                      contaVerdes [Vermelho] = 0
                      contaVerdes (Verde:xs) = 1 + contaVerdes xs 
                      contaVerdes (Amarelo:xs) = 1 + contaVerdes xs
                      contaVerdes (Vermelho:xs) = contaVerdes xs 