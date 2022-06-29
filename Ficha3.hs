data Hora = H Int Int
          deriving Show


type Etapa = (Hora,Hora)
type Viagem = [Etapa]

v :: Viagem 
v = [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]

comparaHora :: Hora -> Hora -> Bool
comparaHora (H h1 m1) (H h2 m2) | h1>h2     = True
                                | h1<h2     = False
                                | m1>m2     = True
                                | otherwise = False

horaValida :: Hora -> Bool
horaValida (H h1 m1) = (h1>=0) && (h1<24) &&
                       (m1>=0) && (m1<60) 

etapaValida :: Etapa -> Bool 
etapaValida (hi,hf) = horaValida hi && horaValida hf 
                      && comparaHora hf hi 

viagemValida :: Viagem -> Bool 
viagemValida []  = True
viagemValida [e] = etapaValida e
viagemValida (e1:e2:t) 
      | etapaValida e1 && comparaHora (fst e2) (snd e1) = viagemValida (e2:t)
      | otherwise                                       = False 

partidaChegada :: Viagem -> (Hora,Hora) 
partidaChegada (h:t) = (fst (h), b ) 
       where b = snd (last t) 

horasParaMinutos :: Hora -> Int
horasParaMinutos (H hora minutos) = hora * 60 + minutos

diferencaHoras :: Hora -> Hora -> Hora
diferencaHoras (H h1 m1) (H h2 m2) = if h1 == h2 then abs (m1 m1)
                                     else abs ((h1-h2) * 60) + abs (m1-m2)  

tempoTotalViagem :: Viagem -> Int 
tempoTotalViagem [] = 0 
tempoTotalViagem (e:es) = (diferencaHoras (fst e) (snd e)) + tempoTotalViagem es 







