{-| 
Module : Main
Description : Módulo Haskell da Tarefa 2 do Projeto do Light Bot
Copyright : João Barreira <joao-mpb@hotmail.com>
            Sofia Carvalho <smgc@live.com.pt>

Um módulo referente à Tarefa 2 do trabalho de Laboratórios de Informática I que consiste no cálculo da próxima posição (após a execução de um comando).

Obs.: Definimos o facto das funções erro darem como resultado o par ((0,0), 'X') como sendo o elemento ilustrativo de que há um erro.
-}

module Main where

import Data.Char

type Orientacao = Char 
type Posicao = (Int, Int)
type Tabuleiro = [String]
type Cmd = Char

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

main = do inp <- getContents
          putStr (outStr(tarefa2(lines inp)))

{- | A função 'tarefa2' é a função principal correspondente à segunda tarefa.

Obs.: Foram utilizadas as seguintes funções auxiliares:

* 'stringParaTuplo' - que recebe uma string correspondente à linha da posição e a converte num triplo;
* 'imprimirValidacao' - que recebe um tuplo correspondente à execução da função 'stringParaTuplo', convertendo-o numa lista de strings;

-}

tarefa2 :: [String] -> [String]
tarefa2 txt = msg
    where tab = takeWhile (all isAlpha) txt -- extrai o tabuleiro (i.e. da linha 1 até à linha anterior à linha da posição)
          (numx, numy, o) = stringParaTuplo (words (txt!!(length txt-2))) -- converte a string da linha da posição num tuplo
          cmds = txt!!(length txt-1) -- extrai a linha de comandos
          res = execCmd tab (numx,numy) o (head cmds) -- executa a função execCmd definida abaixo utilizando os valores anteriores
          msg = if res /= ((0,0), 'X') -- i.e. se não houve erro
                then imprimirValidacao res
                else ["ERRO"]


stringParaTuplo :: [String] -> (Int, Int, Orientacao)
stringParaTuplo s = (read (s!!0)::Int, read (s!!1)::Int, head(s!!2))


imprimirValidacao :: ((Int, Int), Char) -> Tabuleiro
imprimirValidacao ((x,y), o) = [(show x) ++ " " ++ (show y) ++ " " ++ [o]]


{- | A função 'execCmd' é a função que executa os comandos de cada tipo.

Obs.: Foram utilizadas as seguintes funções auxiliares:

* 'virarE' e 'virarD' - que recebe uma orientação e efetua a rotação para a esquerda/direita, dando como output a orientação final.
    >>> virarE 'N'
    'O'
    >>> virarD 'N'
    'E'
* 'procuraTab' - que recebe o tabuleiro e uma posição e dá como output a letra correspondente àquela posição.
    >>> procuraTab ["Aaa","bbb","ccc"] (0,3)
    'A'
* 'avancar' - que recebe uma orientação e uma posição e dá como output as coordenadas da posição que se obtém avançando uma unidade na orientação dada.
    >>> avancar 'N' (0,1)
    (0,2)
-}


execCmd :: Tabuleiro -> Posicao -> Orientacao -> Cmd -> (Posicao,Orientacao)
execCmd tab (x,y) o 'E' = ((x,y), virarE o) 
execCmd tab (x,y) o 'D' = ((x,y), virarD o)
execCmd tab (x,y) o 'L' = if isUpper (procuraTab tab (x,y)) -- verifica se a posição tem luz
                          then ((x,y), o)
                          else ((0,0), 'X')
execCmd tab (x,y) o 'A' = if podeAvancar tab (x,y) o
                          then if toLower(procuraTab tab (x,y)) == toLower(procuraTab tab (avancar o (x,y))) -- verifica se a altura da posição inicial é igual à da posiçao final (para que o robot possa, efetivamente, avançar)
                               then (avancar o (x,y), o)
                               else ((0,0), 'X')
                          else ((x,y), 'X')
execCmd tab (x,y) o 'S' = if podeAvancar tab (x,y) o
                          then if ord (toLower (procuraTab tab (x,y))) - ord (toLower (procuraTab tab (avancar o (x,y)))) ==  -1 || ord (toLower (procuraTab tab (x,y))) - ord (toLower (procuraTab tab (avancar o (x,y)))) >=  1 -- verifica se a posição final tem altura superior a 1 ou inferior em uma ou mais unidades (condições necessárias para que o robot possa saltar)
                               then (avancar o (x,y), o)
                               else ((0,0), 'X')
                          else ((0,0), 'X')

podeAvancar :: Tabuleiro -> Posicao -> Orientacao -> Bool
podeAvancar t (x,y) 'N' = y /= ((length t) - 1)
podeAvancar t (x,y) 'S' = y /= 0
podeAvancar t (x,y) 'E' = x /= ((length (head t)) - 1)
podeAvancar t (x,y) 'O' = x /= 0

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