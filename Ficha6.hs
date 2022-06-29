data BTree a = Empty
             | Node a (BTree a) (BTree a)
        deriving Show

t :: BTree Int
t = Node 7 (Node 5 (Node 3 Empty Empty)
                   (Node 6 Empty Empty)) 
           (Node 8 Empty 
                   (Node 9 Empty Empty))

altura :: BTree a -> Int
altura Empty = 0 
altura (Node _ e d) = 1 + max (altura e) (altura d)

contaNodos :: BTree a -> Int 
contaNodos Empty = 0 
contaNodos (Node _ e d) = 1 + contaNodos e + contaNodos d

folhas :: BTree a -> Int 
folhas Empty = 0 
folhas (Node _ Empty Empty) = 1 
folhas (Node _ e d) = folhas e + folhas d 

prune :: Int -> BTree a -> BTree a 
prune _ Empty = Empty 
prune 0 _     = Empty 
prune a (Node b e d) = Node b (prune (a-1) e) (prune (a-1) d)

path :: [Bool] -> BTree a -> [a] 
path _ Empty = []  
path [] (Node i e d) = [i] 
path (True:is) (Node i e d) = i : path is d 
path (False:is) (Node i e d) = i : path is e 

minimo :: Ord a => BTree a -> a 
minimo (Node i Empty _) = i 
minimo (Node i e d) = minimo e   

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node i Empty d) = d 
semMinimo (Node i e d) = (Node i (semMinimo e) d)  

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL  deriving (Show,Eq)
data Classificacao = Aprov Int | Rep | Faltou deriving Show
type Turma = BTree Aluno --  ́arvore binária de procura (ordenada por número)

turma :: Turma
turma = Node (7,"Ana",ORD,Aprov 16) 
                 (Node (5,"Ze",TE,Rep)
                         (Node (4,"To",ORD,Aprov 18) Empty Empty)
                         (Node (6,"Rui",TE,Aprov 14) Empty Empty))
                 (Node (13,"Te",TE,Aprov 10) Empty Empty)

inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False 
inscNum n (Node (i,_,_,_) e d) | n == i = True 
                               | n > i  = inscNum n d
                               | n < i  = inscNum n e 

inscNome :: Nome -> Turma -> Bool 
inscNome nome Empty = False 
inscNome nome (Node (_,n,_,_) e d) | nome == n = True 
                                   | otherwise = inscNome nome e || inscNome nome d

trabEst :: Turma -> [(Numero,Nome)] 
trabEst Empty = []
trabEst (Node (i,nm,te,_) e d) | te == TE  = trabEst e ++ [(i,nm)] ++ trabEst d 
                               | otherwise = trabEst e ++ trabEst d

-- ou 

trabEst' (Node (i,nm,TE,_) e d) = trabEst' e ++ [(i,nm)] ++ trabEst' d 
trabEst' (Node (i,nm,_,_) e d) = trabEst' e ++ trabEst' d



