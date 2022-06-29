import System.Random 
import Data.Char

---------------------------------

date ::  IO (Int,Int,Int)
date = do d <- randomRIO (1,31)
          m <- randomRIO (1,12)
          a <- randomRIO (0,2021) 
          return (d,m,a)

---------------------------------

bingo :: IO () 
bingo = do print "Comecouuuuuuuuuuuuuu o Bingoooo"
           nums <- jogarBingo [] 
           print nums 

jogarBingo :: [Int] -> IO [Int]
jogarBingo l | length l == 90 = return l
             | otherwise = do  n <- randomRIO (1,90) 
                               print n
                               getChar 
                               let nl = if elem n l 
                                        then l 
                                        else n:l 
                               jogarBingo nl 

chave :: IO (Int,Int,Int,Int)
chave = do c1 <- randomRIO (0,9)
           c2 <- randomRIO (0,9)
           c3 <- randomRIO (0,9)
           c4 <- randomRIO (0,9)
           return (c1,c2,c3,c4)

jogador :: IO (Int,Int,Int,Int)
jogador = do print "Introduz 4 digitos"
             j1 <- getChar
             j2 <- getChar
             j3 <- getChar
             j4 <- getChar 
             return (digitToInt j1,digitToInt j2,digitToInt j3,digitToInt j4) 

iguais :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> (Int,Int)
iguais (c1,c2,c3,c4) (a1,a2,a3,a4) =
       ((length (filter (== True) [c1==a1,c2==a2,c3==a3,c4==a4])),
        (length (filter (== True) [c1==a2,c1==a3,c1==a4,c2==a1,c2==a3,c2==a4,c3==a2,c3==a1,c3==a4]))) 

mastermind :: IO ()
mastermind = do cvs <- chave 
                as <- jogador 
                t <- jogar cvs as 1 
                print ("Finalmente!! " ++ show cvs ++ "em " ++ show t ++ "tentativas")

jogar :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> Int -> IO Int 
jogar cv as t | fst (iguais cv as) == 4 = return t 
              | otherwise = do print ("Tem " ++ show (iguais cv as) ++ "iguais") 
                               as1 <- jogador 
                               jogar cv as1 (t+1) 





























