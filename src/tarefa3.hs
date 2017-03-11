{-| 
Module : Main
Description : Módulo Haskell da Tarefa 3 do Projeto do Light Bot
Copyright : João Barreira <joao-mpb@hotmail.com>
            Sofia Carvalho <smgc@live.com.pt>

Um módulo referente à Tarefa 3 do trabalho de Laboratórios de Informática I que consiste em fazer correr a totalidade da linha de comandos, dando como output as posições onde as luzes foram acesas/desligas, FIM ou INCOMPLETO se tiverem sido ou não ligadas todos as luzes do tabuleiros, respetivamente; e uma contagem de todos os comandos válidos executados
-}

module Main where

import Data.Char
import Data.List

type Orientacao = Char
type Posicao = (Int, Int)
type Tabuleiro = [String]
type Cmd = Char
type ListaCmd = [Char]

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

main = do inp <- getContents
          putStr (outStr(tarefa3(lines inp)))

{- | A função 'tarefa2' é a função principal correspondente à segunda tarefa.

Obs.: Foram utilizadas as seguintes funções auxiliares:

* 'stringParaTuplo' - que recebe uma string correspondente à linha da posição e a converte num triplo;
* 'imprimirValidacao' 
* 'numLampTab'
* 'auxLamp'
* 'res'
* 'remP'
* 'finaLamp'
* 'aux'
* 'numLampAcesas'

-}

tarefa3 :: [String] -> [String]
tarefa3 txt = msg
    where tab = takeWhile (all isAlpha) txt
          (numx, numy, o) = stringParaTuplo (words (txt!!(length txt-2)))
          cmds = txt!!(length txt-1)
          numLampTab = numeroLampTab tab 0
          auxLamp = map (\(x, y) -> (x, (length tab) - y -1)) numLampTab
          res = execListaCmd tab (numx,numy) o 0 cmds auxLamp
          remP = removeP res
          finaLamp = if null res then []
                     else snd (last res)
          aux = map (\(((x, y), c), l) -> ((x, y), c)) remP
          numLampAcesas = length finaLamp
          msg = if (numLampAcesas == 0)
                then (imprimirValidacao aux False)
                else (imprimirValidacao aux True)

stringParaTuplo :: [String] -> (Int, Int, Orientacao)
stringParaTuplo s = (read (s!!0)::Int, read (s!!1)::Int, head(s!!2))

imprimirValidacao :: [((Int, Int), Int)] -> Bool -> Tabuleiro
imprimirValidacao [] False = ["FIM 0"]
imprimirValidacao [] True = ["INCOMPLETO"]
imprimirValidacao [((x, y), o)] False = [(show x) ++ " " ++ (show y)] ++ ["FIM" ++ " " ++ (show o)]
imprimirValidacao [((x, y), o)] True = [(show x) ++ " " ++ (show y)] ++ ["INCOMPLETO"]
imprimirValidacao (((x,y), o):xs) t = (((show x) ++ " " ++ (show y))) : (imprimirValidacao xs t)

{- | A função 'execCmd' é a função que executa a lista de comandos. É ela que faz a maior parte do "trabalho" pois além de executar estes comandos, testas as condições em que isso não acontece, em que os comandos seriam executados fora do tabuleiro e ainda efetua a conta dos comandos válidos executados (através do Int chamado pela letra c).

Obs.: Foram utilizadas as seguintes funções auxiliares:

* 'virarE' e 'virarD' - que recebe uma orientação e efetua a rotação para a esquerda/direita, dando como output a orientação final.
* 'procuraTab' - que recebe o tabuleiro e uma posição e dá como output a letra correspondente àquela posição.
* 'avancar' - que recebe uma orientação e uma posição e dá como output as coordenadas da posição que se obtém avançando uma unidade na orientação dada.
* 'removeAdd'
* 'podeAvancar'

Obs.: Os valores de -1 servem apenas como marcação e serão removidos posteriormente pela função removeP.
-}

execListaCmd :: Tabuleiro -> Posicao -> Orientacao -> Int -> ListaCmd -> [(Int, Int)]-> [((Posicao, Int), [(Int, Int)])]
execListaCmd _ _ _ _ _ [] = []
execListaCmd tab (x, y) o c [] _ = []
execListaCmd tab (x,y) o c (h:t) l = if h=='E'
                                   then ((( -1, -1), c + 1), l) : (execListaCmd tab (x,y) (virarE o) (c+1) t l)
                                   else if h=='D'
                                        then (((-1, -1), c + 1), l) : (execListaCmd tab (x,y) (virarD o) (c+1) t l)
                                        else if h=='L'
                                             then if isUpper (procuraTab tab (x,y))
                                                  then (((x,y), c + 1), (removeAdd (x, y) l)) : (execListaCmd tab (x,y) o (c+1) t (removeAdd (x, y) l))
                                                  else execListaCmd tab (x,y) o c t (removeAdd (x, y) l)
                                             else if podeAvancar tab (x, y) o
                                                  then if h=='A'
                                                       then if toLower(procuraTab tab (x,y)) == toLower(procuraTab tab (avancar o (x,y)))
                                                            then (((-1, -1), c + 1), l) : (execListaCmd tab (avancar o (x,y)) o (c+1) t l)
                                                            else execListaCmd tab (x,y) o c t l
                                                       else if h=='S'
                                                            then if ord (toLower (procuraTab tab (x,y))) - ord (toLower (procuraTab tab (avancar o (x,y)))) ==  -1 || ord (toLower (procuraTab tab (x,y))) - ord (toLower (procuraTab tab (avancar o (x,y)))) >=  1
                                                                 then (((-1, -1), c + 1), l) : (execListaCmd tab (avancar o (x,y)) o (c+1) t l)
                                                                 else execListaCmd tab (x,y) o c t l
                                                            else execListaCmd tab (x,y) o c t l
                                                  else execListaCmd tab (x,y) o c t l

podeAvancar :: Tabuleiro -> Posicao -> Orientacao -> Bool
podeAvancar t (x,y) 'N' = y /= ((length t) - 1)
podeAvancar t (x,y) 'S' = y /= 0
podeAvancar t (x,y) 'E' = x /= ((length (head t)) - 1)
podeAvancar t (x,y) 'O' = x /= 0

removeAdd :: Posicao -> [(Int, Int)] -> [(Int, Int)]
removeAdd p l | elem p l  = filter (\h -> h /= p) l
              | otherwise = l ++ [p]

numeroLampTab :: Tabuleiro -> Int -> [(Int, Int)]
numeroLampTab [] _ = []
numeroLampTab (h:t) l = (numeroLampLinha h l 0) ++ (numeroLampTab t (l + 1)) 

numeroLampLinha :: String -> Int -> Int -> [(Int, Int)]
numeroLampLinha [] _ _ = []
numeroLampLinha (h:t) l c = if isUpper h
                            then (c, l) : (numeroLampLinha t l (c+1))
                            else numeroLampLinha t l (c + 1)

removeP :: [((Posicao, Int), [(Int, Int)])] -> [((Posicao, Int), [(Int, Int)])]
removeP l = filter (\(((x, y), c), l) -> x /= -1 ) l

procuraTab :: Tabuleiro -> Posicao -> Char
procuraTab t (a,b) = (t!!((length t) - 1 - b)!!a)

virarE :: Orientacao -> Orientacao
virarE c | c=='N' = 'O'
         | c=='S' = 'E'
         | c=='E' = 'N'
         | otherwise = 'S'

virarD :: Orientacao -> Orientacao
virarD c | c=='N' = 'E'
         | c=='S' = 'O'
         | c=='E' = 'S'
         | otherwise = 'N'

avancar :: Orientacao -> Posicao -> Posicao
avancar c (a,b) | c=='N' = (a,(b+1))
                | c=='S' = (a,(b-1))
                | c=='E' = ((a+1),b)
                | otherwise = ((a-1),b)
