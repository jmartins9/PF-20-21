data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

ag :: Agenda 
ag = [ ("Ana" , [Tlm 9311111])
     , ("Ze" , [Trab 25311111 , Casa 25811111 , Email "ze@gmail.com"])
     ]

acresEmail :: Nome -> String -> Agenda -> Agenda
acresEmail n e []       = [(n,[Email e])]
acresEmail n e (c:cs) 
           | n == fst c = (n,Email e : snd c) : cs 
           | otherwise  = c : acresEmail n e cs 

verEmails :: Nome -> Agenda -> Maybe [String]   
verEmails _ []        = Nothing 
verEmails nc ((n,c):cs) 
          | n == nc   = Just (emails c)  
          | otherwise = verEmails nc cs 

emails :: [Contacto] -> [String]           
emails []           = []
emails (Email e:cs) = e:emails cs 
emails (_:cs)       = emails cs

consTelefs :: [Contacto] -> [Integer] 
consTelefs []          = []
consTelefs (Tlm c:cs)  = c : consTelefs cs 
consTelefs (Trab c:cs) = c : consTelefs cs 
consTelefs (Casa c:cs) = c : consTelefs cs 
consTelefs (_:cs)      = consTelefs cs

consTelefs' []     = [] 
consTelefs' (c:cs) = case c of 
                       Tlm  t -> t : consTelefs' cs 
                       Trab t -> t : consTelefs' cs 
                       Casa t -> t : consTelefs' cs 
                       _      -> consTelefs' cs 

casa :: Nome -> Agenda -> Maybe Integer 
casa _ [] = Nothing 
casa n ((nc,c):cs) | n == nc   = contactosCasa c 
                   | otherwise = casa n cs  

contactosCasa :: [Contacto] -> Maybe Integer
contactosCasa []          = Nothing
contactosCasa (Casa c:cs) = Just c 
contactosCasa (_:cs)      = contactosCasa cs                       


-- 5

data Movimento = Credito Float | Debito Float
               deriving Show

data Data = D Int Int Int
          deriving Show 

data Extracto = Ext Float [(Data,String,Movimento)]
              deriving Show

mesNovembro :: Extracto 
mesNovembro = Ext 745.4 [ (D 1 11 2020,"edp",Debito 33.1)
                         , (D 2 11 2020,"sal",Credito 500)
                         , (D 10 11 2020,"MB",Debito 80)
                         ]

extValor :: Extracto -> Float -> [Movimento] 
extValor (Ext _ mvs) v = mvsSup mvs v 

mvsSup :: [(Data,String,Movimento)] -> Float -> [Movimento]
mvsSup [] _ = []
mvsSup ((_,_,Credito x):t) v | x > v = (Credito x) : mvsSup t v
                             | otherwise = mvsSup t v
mvsSup ((_,_,Debito x):t) v | x > v = (Debito x) : mvsSup t v 
                            | otherwise = mvsSup t v

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ mvs) descs = filtromvs mvs descs 

filtromvs [] _ = []
filtromvs ((d,dc,m):t) descs 
          | elem dc descs = (d,m) : filtromvs t descs 
          | otherwise     = filtromvs t descs 































