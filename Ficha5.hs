import Data.List

any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) | p x = True 
              | otherwise = any' p xs 

zipWith' :: (a->b->c) -> [a] -> [b] -> [c] 
zipWith' f _ [] = [] 
zipWith' f [] _ = [] 
zipWith' f (x:xs) (y:ys) = (f x y ) : zipWith' f xs ys

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' _ [] = [] 
takeWhile' p (h:t) | p h       = h : takeWhile' p t
                   | otherwise = [] 

dropWhile' :: (a->Bool) -> [a] -> [a] 
dropWhile' _ [] = [] 
dropWhile' p (h:t) | p h       = dropWhile' p t 
                   | otherwise = (h:t)

span' :: (a-> Bool) -> [a] -> ([a],[a]) 
span' p [] = ([],[])
span' p (h:t) | p h = (h:lt,ld)
              | otherwise = ([],h:t)
              where (lt,ld) = span' p t

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a] 
deleteBy' f _ [] = []
deleteBy' f a (h:t) | f a h = t 
                    | otherwise = h : deleteBy' f a t 

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [x] = [x] 
sortOn' f (x:y:xs) | f x <= f y = x : sortOn' f (y:xs)
                   | otherwise  = insere f x (sortOn' f (y:xs))

insere f a (x:xs) | f a <= f x = (a : x : xs)
                  | otherwise  = x : insere f a xs                  


type Mat a = [[a]]

m = [[1,2,3], [0,4,5], [0,0,6]]

dimOK :: Mat a -> Bool
dimOK [] = True 
dimOK m = let d = length (head m)
          in dimOK' m d 

dimOK' :: Mat a -> Int -> Bool 
dimOK' m d = and (map ( \ l -> length l == d) m)

dimMat :: Mat a -> (Int,Int) 
dimMat [] = (0,0)
dimMat m = ( length m , length (head m) )

addMat :: Num a => Mat a -> Mat a -> Mat a   
addMat [] l = l
addMat l [] = l
addMat (x:xs) (y:ys) = (zipWith (+) x y) : addMat xs ys

transpose' :: Mat a -> Mat a 
transpose' [] = [] 
transpose' m | null (head m) = []
             | otherwise = primeiraCol m : transpose' (restantesCols m) 
             
primeiraCol :: Mat a -> [a] 
primeiraCol m = map head m

restantesCols :: Mat a -> Mat a 
restantesCols m = map tail m
 
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat [] l = [] 
multMat l [] = [] 
multMat m1 m2 = let tm = transpose' m2 
                in multMat' m1 tm 

multMat' :: Num a => Mat a -> Mat a -> Mat a
multMat' [] _ = []
multMat' (y:ys) m = map (\x -> sum ( zipWith (*) y x )) m : multMat' ys m









































