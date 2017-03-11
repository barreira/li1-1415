{-| 
Module : Main
Description : Módulo Haskell da Tarefa 4 do Projeto do Light Bot
Copyright : João Barreira <joao-mpb@hotmail.com>
            Sofia Carvalho <smgc@live.com.pt>

Um módulo referente à Tarefa 4 do trabalho de Laboratórios de Informática I que consiste em sintetizar uma linha de comandos que serão executados pelo robot. O objetivo é que o programa sintetizado consigo que o robot acenda todas as lâmpadas do tabuleiro, partindo da sua posição inicial.

-}

module Main where

import Data.Char
import Data.List

{-| Definição do tipo 'Posicao' que é um par de inteiros (abcissa e ordenada de uma posição qualquer).

-}

type Posicao = (Int,Int)

{-| Definição do tipo 'Orientacao' que é um carater (iremos utilizar 'N','S','E e 'O').

-}

type Orientacao = Char

{-| Definição do tipo 'Cmd' (i.e. Comando) que é um carater (iremos utilizar 'A','S','L','E' e 'D').

-}

type Cmd = Char

{-| Definição do tipo 'Tabuleiro' que é uma lista de strings (com cada string correspondendo a uma linha).

-}

type Tabuleiro = [String]

{-| Função 'outStr' que se encarrega apenas de moldar o input para ser trabalhado pelo progama (recebe o tabuleiro como sendo uma lista de string e aplica-lhe a função pré-definida 'unlines', dando como output uma só string com separadores do tipo /n).

-}

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

{-| Este pedaço de código serve para ler do ficheiro txt o conteúdo do jogo.

-}

main = do inp <- getContents
          putStr (outStr(tarefa4(lines inp)))

{-| A função 'tarefa4' é a função principal da tarefa. É dela que resulta a linha de comandos que o robot irá executar.

*@tab = takeWhile (all isALpha) txt@ - ao receber o tabuleiro completo retira as linhas que são letras (ou, por outra palavras, exclui a linha das posições (penúltima linha) e a dos comandos (que foi omitida para esta tarefa)).

*@tabInv = reverse (takeWhile (all isAlpha) txt)@ - inverte o tabuleiro para que, ao serem retiradas as posições do tabuleiro com lâmpada, elas venham pela ordem natural correspondente à disposição do tabuleiro.

*@listaPosL = posicoesComL tabInv@ - aplica a função 'posicoesComL' ao tabuleiro invertido (i.e. vai ao tabuleiro e retira as coordenadas das posições que têm lâmpada)

*@(numx, numy, o) = stringParaTuplo (words (txt!!(length txt-1)))@ - é aplicada função pré-definida "words" à linha das posições criando uma lista de strings (divindindo as várias strings onde ouver um espaço) que já poderá ser usada pela função 'stringParaTuplo' que irá pegar nessa lista de strings e dar como output um tuplo com a coordenada x inicIal, a coordenada y inical e a orientação inicial.

*@msg = [moveAcende (numx,numy) o listaPosL tab]@ - aplica a função 'moveAcende' (que irá fazer com que o robot se desloque ao longo do tabuleiro e vá acendendo as posições que possuam lâmpada); recebe como input um par com as coordenadas iniciais do robot, a orientação inicial, a lista com as coordenadas das posições do tabuleiro com lâmpada e o tabuleiro (não invertido).

-}

tarefa4 :: [String] -> [String]
tarefa4 txt = msg
    where tab = takeWhile (all isAlpha) txt
          tabInv = reverse (takeWhile (all isAlpha) txt)
          listaPosL = posicoesComL tabInv
          (numx, numy, o) = stringParaTuplo (words (txt!!(length txt-1)))
          msg = [moveAcende (numx,numy) o listaPosL tab]

{-| A função 'stringParaTuplo' recebe com input uma lista de strings e dá como output um tuplo com a coordenada x inical, a coordenada y inical e a orientação inicial.

*@(read (s!!0)::Int, read (s!!1)::Int, head(s!!2))@ - recebendo a linha das posições do tabuleiro (e.g. "4 5 N"), lê (i.e. utiliza a função pré-definida "read") o elemento de índice 0 da string (o primeiro elemento da string; nesta caso, o número 4) e interpreta-o como um inteiro (para poder ser usado como tal posteriormente); lê o elemento de índice 1 (o segundo elemento da string; neste caso, o número 5) interpretando-o também como um inteiro; por fim, lê o elemento de índice 2 (o terceiro elemento da string, neste caso, a letra N), interpretando-o, por default, como um char.

-}

stringParaTuplo :: [String] -> (Int, Int, Orientacao)
stringParaTuplo s = (read (s!!0)::Int, read (s!!1)::Int, head(s!!2))

{-| As funções 'posicoesComL', 'percorreL' e 'funcaoExtL' atuam em conjunto, recebendo o tabuleiro e dando como output uma lista com as coordenadas das posições que possuem lâmpada.

*@posicoesComL t = funcaoExtL 0 t@ - esta função combina as duas anteriores, recebendo um tabuleiro, percorrendo tanto as linhas como as colunas e dando como output uma lista com as coordenadas das posições que possuem lâmpada; para isso, chama a função 'funcaoExtL' com o contador de linhas a 0 (para esta mesma função começar a atuar a partir da primeira coluna do tabuleiro).

-}

posicoesComL :: Tabuleiro -> [Posicao]
posicoesComL [] = []
posicoesComL t = funcaoExtL 0 t

{-| *@percorreL l c (x:xs) = if isUpper x 
                         then (c,l) : (percorreL l (c+1) xs)
                         else percorreL l (c+1) xs @ - esta função percorre uma linha do tabuleiro e vai testando as letras do mesmo; caso encontre uma letra maiúscula (i.e. posição com lâmpada), regista um par com as coordenadas da posição onde está e continua a percorrer essa mesma linha (avançando uma coluna).

-} 

percorreL :: Int -> Int -> String -> [Posicao]
percorreL _ _ [] = []
percorreL l c (x:xs) = if isUpper x 
                       then (c,l) : (percorreL l (c+1) xs)
                       else percorreL l (c+1) xs

{-| *@funcaoExtL l (x:xs) = (percorreL l 0 x) ++ (funcaoExtL (l+1) xs)@ - esta função chama a função 'percorreL' (que por sua vez irá percorrer a linha "l" do tabuleiro começando na coluna 0 - porque é chamada com o contador de colunas a 0 -, e regista o par das coordenadas onde houver letras maiúsculas (i.e. posição com lâmpada)); depois a função 'funcaoExtL' atua recursivamente, chamando novamente a função 'percorreL' mas agora para a linha "l+1" (a linha a seguir).

-}

funcaoExtL :: Int -> Tabuleiro -> [Posicao]
funcaoExtL _ [] = []
funcaoExtL l (x:xs) = (percorreL l 0 x) ++ (funcaoExtL (l+1) xs)

{-| A função 'moveAcende' atua recursivamente fazendo com que o robot se desloque ao longo do tabuleiro e vá acendendo as posições que possuam lâmpada. Para isso utiliza a função 'move2' definida posteriormente e explicada mais à frente nos comentários.

*Primeiro a função vai buscar a lista dos comandos necessários para o robot se desclocar desde a posição inicial até à posição final (a primeira (ou a head) da lista posições com lâmpada) e, após isso, junta-lhe uma outra lista com apenas o comando 'L' (para acender a lâmpada) (@(snd (move2 (a,b) o (x,y) t) ++ ['L'])@).

*Depois a função 'moveAcende' atua recursivamente (@moveAcende (x,y) (fst (move2 (a,b) o (x,y) t)) xs t@) na cauda da lista das posições com luz utilizando para isso como orientação a resultante do passo anterior (da deslocação do robot da posição inicial até à primeira lâmpada; @(fst (move2 (a,b) o (x,y) t))@)

-}

moveAcende :: Posicao -> Orientacao -> [Posicao] -> Tabuleiro -> [Cmd]
moveAcende _ _ [] _ = []
moveAcende (a,b) o ((x,y):xs) t = (snd (move2 (a,b) o (x,y) t) ++ ['L']) ++ moveAcende (x,y) (fst (move2 (a,b) o (x,y) t)) xs t

{-| A função 'mudaOrient', recebendo uma orientação inicial e uma final, dá como output uma lista de comandos que serão necessários para que o robot gire da orientação inicial para a final.

Exemplos de utilização:

>>> mudaOrient 'N' 'E'
>>> ['D']

>>> mudaOrient 'N' 'S'
>>> ['D','D']

>>> mudaOrient 'N' 'N'
>>> []

-}

mudaOrient :: Orientacao -> Orientacao -> [Cmd]
mudaOrient o p = if o=='N' 
                 then if p=='N'
                      then []
                      else if p=='S'
                           then ['D','D']
                           else if p=='E'
                                then ['D']
                                else ['E']
                 else if o=='S'
                      then if p=='N'
                           then ['D','D']
                           else if p=='S'
                                then []
                                else if p=='E'
                                     then ['E']
                                     else ['D']
                      else if o=='E'
                           then if p=='N'
                                then ['E']
                                else if p=='S'
                                     then ['D']
                                     else if p=='E'
                                          then []
                                          else ['D','D']
                           else if o=='O'
                                then if p=='N'
                                     then ['D']
                                     else if p=='S'
                                          then ['E']
                                          else if p=='E'
                                               then ['D','D']
                                               else []
                                else []

{-| A função 'move2' recebe como input duas posições (uma inicial e uma final), uma orientação inicial, o tabuleiro e dá como output um par com a orientação final e a lista de comandos que são necessários para o robot se deslocar da posição inicial para a final. Este movimento do robot é dividido em duas partes: primeiro o robot acerta a coordenada x e só depois acerta a y.

*@(ox,cx) = moveX2 (a,b) o (x,b) t@ - primeira parte do movimento da função principal em que o robot acerta as coordenadas x; para isso, utiliza a função 'moveX' para se mover da posição (a,b) (posição inicial) para a posição (x,b) (posição intermédia; posição com a mesma abcissa da posição final). Guarda na forma de um par a lista de comandos necessários para que o robot efetue este movimento, bem como a orientação com que irá finalizar o movimento.

*@(oy,cy) = moveY2 (x,b) ox (x,y) t@ - segunda e última parte do movimento da função principal 'move2' em que o robot acerta as coordenadas y; para isso, utiliza a função 'moveY' para se mover da posição (x,b) (posição intermédia) para a posição (x,y) (posição final), no entanto, como parte da posição intermédia após acertar as coordenadas x, a orientação utilizada no input não será a orientação incial mas sim a orientação que resulta do acerto das coordenadas x (que veio da primeira parte da função 'move2'). Guarda mais uma vez um par com a lista de comandos necessários para que o robot efetue este movimento, bem como a orientação com que irá finalizar o movimento.

*@move2 (a,b) o (x,y) t = (oy,cx++cy)@ - parte da função em que nos é dado como output um par com a orientação final do acerto das coordenadas y (orientação final do movimento), bem como a junção das listas dos comandos necessários para efetuar tanto o acerto das coordenadas x como o das coordenadas y.

-}

move2 :: Posicao -> Orientacao -> Posicao -> Tabuleiro -> (Orientacao,[Cmd])
move2 (a,b) o (x,y) t = (oy,cx++cy)
    where (ox,cx) = moveX2 (a,b) o (x,b) t
          (oy,cy) = moveY2 (x,b) ox (x,y) t

{-| A função 'moveX2' é uma função que recebe como input duas posições (uma inicial e uma final), uma orientação inicial, o tabuleiro e dá como output um par com a orientação final e a lista de comandos que são necessários para o robot se deslocar da posição inicial para a final (comandos esses que são gerados pela função 'mudaX2'). Esta função apenas acerta as coordenadas x.

Explicação da função:

*Primeiro a função verifica se a abcissa da posição inicial é diferente da abcissa da posição final (@if (a /= x)@). Se não for diferente esta função não atua pois as posições têm abcissas iguais e, por isso, dá como output o par (o,[]), visto que orientação final será a mesma que a inicial e a lista de comandos será vazia pois, de facto, não existiu qualquer movimento do robot.

*Se as abcissas forem diferentes a função verifica se a abcissa final tem valor superior à inicial (@then if x > a@) ou inferior (@else if x < a@).

*Se tiver valor superior a função verifica se ao ser aplicada a função 'avancar' (correspondente ao comando de Avançar) com a orientação definada como 'E' (pois o robot quer ir para uma posição com abcissa superior, logo terá de se orientar para Este) a abcissa da posição resultante continua a ser menor ou igual à abcissa final (i.e. se ao avançar o robot não ultrapassa a posição final) (@(fst (avancar 'E' (a,b)) <= x)@).

*Se isto acontecer, o programa dá-nos como output um par com a orientação final (que neste caso é 'E' de Este) e com a junção da lista de comandos necessários para que o robot ajuste a sua orientação para Este e da lista de comandos necessários para que o robot se desloque da posição inicial para a posição final (@('E', mudaOrient o 'E' ++ mudaX2 (a,b) (x,y) 'E' t)@).

*Se isto não acontecer, o programa dará como output um par com a orientação final (que, neste caso será igual à inicial) e uma lista de comandos vazia pois, de facto, não existiu qualquer movimento do robot.

OBS.: O caso da posição final estar à esquerda da posição inicial (i.e. a sua abcissa ser inferior à da posição inicial) está definido análogamente na função (apenas com a diferença no sinais de maior/menor e com a utilização da orientação Oeste em vez de Este).

-}

moveX2 :: Posicao -> Orientacao -> Posicao -> Tabuleiro -> (Orientacao,[Cmd])
moveX2 (a,b) o (x,y) t = if (a /= x)
                         then if x > a
                              then if (fst (avancar 'E' (a,b)) <= x)
                                   then ('E', mudaOrient o 'E' ++ mudaX2 (a,b) (x,y) 'E' t)
                                   else (o,[])
                              else if x < a
                                   then if (fst (avancar 'O' (a,b)) >= x)
                                        then ('O', mudaOrient o 'O' ++ mudaX2 (a,b) (x,y) 'O' t)
                                        else (o,[])
                                   else (o,[])
                         else (o,[])

{-| A função 'mudaX2' é uma função que recebe como input duas posições (uma inicial e uma final), uma orientação inicial, o tabuleiro e dá como output a lista de comandos que são necessários para o robot se deslocar da posição inicial para a final. Esta função apenas acerta as coordenadas x.

*Primeiro a função verifica se a abcissa da posição inicial é diferente da abcissa da posição final (@if (a /= x)@). Se não for diferente esta função não atua pois as posições têm abcissas iguais, dando como output uma lista de comandos vazia (@else []@) porque, na verdade, não existiu qualquer movimento do robot.

*Se as abcissas forem diferentes, a função verifica se ao ser aplicada a função 'avancar' (correspondente ao comando de Avançar) o robot se desloca de duas posições no mesmo nível (exemplo: de uma posição com a letra b para outra posição com a mesma letra. Para isso utiliza a função pré-definida 'toLower' nas letras das posições inicial e final, letras essas que serão obtidas utilizando a função 'procuraTab' por sua vez aplicada a à posição (a,b) e à posição resultante da execução da função (avançar o (a,b)) (@then if toLower(procuraTab t (a,b)) == toLower(procuraTab t (avancar o (a,b)))@).

*Se isto acontecer (i.e. se as posições estiverem no mesmo nível), a função irá dar como output a junção da lista com o comando 'A' de Avançar com a lista de comandos resultante da chamada recursiva desta mesma função 'mudaX2' que desta vez é chamada utilizando a posição que se obtém da posição inicial executando a função 'avancar' (em vez da própria posição inicial) e a posição final (x,y) (@then ['A'] ++ mudaX2 (avancar o (a,b)) (x,y) o t@).

*Se isto não acontecer (i.e. se as posições não estiverem no mesmo nível), a função verifica se esta divergência de níveis é, quando muito, de apenas um nível para cima (ou infinitos níveis para baixo). Para isso utiliza mais uma vez as funções 'toLower' e 'procuraTab' mas verifica, utilizando a função pré-definida 'ord' se a diferença de níveis é de apenas um para cima (@ord (toLower (procuraTab t (a,b))) - ord (toLower (procuraTab t (avancar o (a,b)))) ==  -1@) ou de infinitos níveis para baixo (@ord (toLower (procuraTab t (a,b))) - ord (toLower (procuraTab t (avancar o (a,b)))) >=  1@).

*Se isto acontecer o robot estará em condições de poder executar o comando 'S' de Saltar. Assim, esta função irá dar como output a junção da lista com o comando 'S' de Saltar com a lista de comandos resultante da chamada recursiva desta mesma função 'mudaX2' que desta vez é chamada utilizando a posição que se obtém da posição inicial executando a função 'avancar' (em vez da própria posição inicial) e a posição final (x,y) (@then ['S'] ++ mudaX2 (avancar o (a,b)) (x,y) o t@).

*Se isto não acontecer (i.e. a posição final está mais do que um nível acima da posição inicial) o robot não conseguirá nem saltar nem avançar e, por isso, a função irá dar como output uma lista de comandos vazia (@else []@).

-} 
mudaX2 :: Posicao -> Posicao -> Orientacao -> Tabuleiro -> [Cmd]
mudaX2 (a,b) (x,y) o t = if (a /= x)
                         then if toLower(procuraTab t (a,b)) == toLower(procuraTab t (avancar o (a,b)))
                              then ['A'] ++ mudaX2 (avancar o (a,b)) (x,y) o t
                              else if ord (toLower (procuraTab t (a,b))) - ord (toLower (procuraTab t (avancar o (a,b)))) ==  -1 || ord (toLower (procuraTab t (a,b))) - ord (toLower (procuraTab t (avancar o (a,b)))) >=  1
                                   then ['S'] ++ mudaX2 (avancar o (a,b)) (x,y) o t
                                   else []
                         else []

{-| A função 'moveY2' é uma função definida análogamente à função 'moveX2', sendo que agora acertará as coordenadas y e não as x e utilizará as orientações Norte/Sul em vez de Este/Oeste.

-}

moveY2 :: Posicao -> Orientacao -> Posicao -> Tabuleiro -> (Orientacao,[Cmd])
moveY2 (a,b) o (x,y) t = if (b /= y)
                         then if y > b
                              then if (snd (avancar 'N' (a,b)) <= y)
                                   then ('N', mudaOrient o 'N' ++ mudaY2 (a,b) (x,y) 'N' t)
                                   else (o,[])
                              else if y < b
                                   then if (snd (avancar 'S' (a,b)) >= y)
                                        then ('S', mudaOrient o 'S' ++ mudaY2 (a,b) (x,y) 'S' t)
                                        else (o,[])
                                   else (o,[])
                         else (o,[])

{-| A função 'mudaY2' é uma função definida análogamente à função 'mudaX2', sendo que agora acertará as coordenadas y e não as x.

-}

mudaY2 :: Posicao -> Posicao -> Orientacao -> Tabuleiro -> [Cmd]
mudaY2 (a,b) (x,y) o t = if (b /= y)
                         then if toLower(procuraTab t (a,b)) == toLower(procuraTab t (avancar o (a,b)))
                              then ['A'] ++ mudaY2 (avancar o (a,b)) (x,y) o t
                              else if ord (toLower (procuraTab t (a,b))) - ord (toLower (procuraTab t (avancar o (a,b)))) ==  -1 || ord (toLower (procuraTab t (a,b))) - ord (toLower (procuraTab t (avancar o (a,b)))) >=  1
                                   then ['S'] ++ mudaY2 (avancar o (a,b)) (x,y) o t
                                   else []
                         else []

{-| A função 'avancar' recebe uma orientação e uma posição e dá como output as coordenadas da posição que se obtém avançando uma unidade na orientação dada.

-}

avancar :: Orientacao -> Posicao -> Posicao
avancar c (a,b) | c=='N' = (a,(b+1))
                | c=='S' = (a,(b-1))
                | c=='E' = ((a+1),b)
                | otherwise = ((a-1),b)

{-| A função 'procuraTab' recebe o tabuleiro e uma posição e dá como output a letra correspondente àquela posição.

-}

procuraTab :: Tabuleiro -> Posicao -> Char
procuraTab t (a,b) = (t!!((length t) - 1 - b)!!a)