{-| 
Module : Main
Description : Módulo Haskell da Tarefa 1 do Projeto do Light Bot
Copyright : João Barreira <joao-mpb@hotmail.com>
            Sofia Carvalho <smgc@live.com.pt>

Um módulo referente à Tarefa 1 do trabalho de Laboratórios de Informática I que consiste na validação da totalidade do tabuleiro do jogo /Light Bot/.
-}

module Main where

import Data.Char

type Tabuleiro = [String]

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

main = do inp <- getContents
          putStr (outStr(tarefa1(lines inp)))

{- | A função 'tarefa1' é a função principal correspondente à primeira tarefa. 

Obs.: Definimos o facto das funções erro darem valor 0 como sendo o elemento identificativo de que não há erro (quando há, é nos mostrada a linha onde tal erro começa).

-}

tarefa1 :: [String] -> [String]
tarefa1 [] = ["1"]
tarefa1 txt = [msg]
    where tab = takeWhile (all isAlpha) txt {-- ^ extrai o tabuleiro (i.e. da linha 1 até à linha anterior à linha da posição) -}
          alturaTab = length tab {-- ^ calcula a altura do tabuleiro -}
          linhapos = txt!!(alturaTab) {-- ^ extrai a linha da posição -}
          linhaprog = txt!!(alturaTab + 1) {-- ^ extrai a linha dos comandos -}
          erroTab = validaTab tab {-- ^ definição dos casos de erro nas linhas do tabuleiro -}
          erroPos = if validaLPos (length (head tab)) alturaTab linhapos
                    then 0
                    else alturaTab + 1 {-- ^ definição dos casos de erro na linha de posição -}
          erroProg = if validaProg linhaprog
                     then 0
                     else length txt {-- ^ definição dos casos de erro na linha de comandos -}
          erroResto = if length txt > (alturaTab + 2) 
                      then (alturaTab + 3)
                      else 0 {-- ^ definição dos casos de erro caso existam mais linhas para além da linha de comandos -}
          msg = if erroTab > 0 {-- ^ se tiver erro (i.e. se a função erro tiver um valor superior a 0)... -}
                then show erroTab {-- ^ ...então mostra o erro em questão -}
                else if erroPos > 0
                     then show erroPos
                     else if erroProg > 0
                          then show erroProg
                          else if erroResto > 0
                               then show erroResto
                               else "OK" {-- ^ se não houverem erros, é mostrada a mensagem "OK" -}

{- | A função 'validaTab' é a função responsável pela validação das linhas do tabuleiro (i.e. da primeira linha à linhas imediatamente anterior à linha de posição).

Obs.: Foi utilizada como função auxiliar a função 'testaLetras'
-}

validaTab :: Tabuleiro -> Int
validaTab [] = 1
validaTab (x:xs) = aux (length x) 1 (x:xs)
      where aux tam n [] = 0
            aux tam n (y:ys) = if (length y) == tam && testaLetras y
                               then aux tam (n+1) ys
                               else n
testaLetras [] = True
testaLetras (x:xs) = isAlpha x && testaLetras xs {-- ^ recebe uma string e verifica se os seus elementos são letras -}

{- | A função 'validaLPos' é a função responsável pela validação da linha de posição (e.g. "1 0 S"). É uma função do tipo /case/ que verifica se:

1. o último elemento da string (orientação) é um "N", um "S", um "E", ou um "O";
2. os primeiros dois elementos da string (xpos e ypos) são dígitos e se estão dentro das dimensões do tabuleiro (i.e. se são menores que o número de colunas e linhas, respetivamente);
3. existem apenas dois carateres espaço na string (e.g. de forma a que não haja um espaço imediatamente antes da coordenada x).

Obs.: Foram utilizadas as seguintes funções auxiliares:

* 'contaEspacos' - que recebe uma string e conta os carateres espaço nela presentes;
* 'testaDigitos' - que recebe uma string e verifica se os seus elementos são digitos.

-}

validaLPos :: Int -> Int -> String -> Bool
validaLPos _ _ [] = False
validaLPos xdim ydim lpos = case words lpos of 
                            [xpos, ypos,[o]] -> elem o "NSEO" && (testaDigitos xpos && read xpos < xdim) && (testaDigitos ypos && read ypos < ydim) && contaEspacos lpos == 2
                            _ -> False

contaEspacos :: String -> Int
contaEspacos [] = 0
contaEspacos (x:xs) | x==' ' = 1 + contaEspacos xs
                    | otherwise = contaEspacos xs

testaDigitos [] = True
testaDigitos (x:xs) = isDigit x && testaDigitos xs

{- | A função 'validaProg' é a função responsável pela validação da linha dos comandos.

Para isso, foi utilizada a função predefinida /all/ que testa se os elementos da string são "A", "S", "D", "E", ou "L".

= Exemplos de utilização:

>>> validaProg "LEDSA"
True
>>> validaProg "LEDSAP"
False
>>> validaProg "LEDSA 1"
False

-}

validaProg :: String -> Bool
validaProg = all (\x -> elem x "ASDEL")