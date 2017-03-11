{-| 
Module : Main
Description : Módulo Haskell da Tarefa 4 do Projeto do Light Bot
Copyright : João Barreira <joao-mpb@hotmail.com>
            Sofia Carvalho <smgc@live.com.pt>

Um módulo referente à Tarefa 5 do trabalho de Laboratórios de Informática I que consiste na visualização do jogo. De acordo com os requisitos mínimos, este programa fornece a visualização do tabuleiro, das posições com luz e da posição inicial do robot.

OBS.: Não nos foi possível adicionar ao programa a possibilidade de animação dos movimentos, i.e. da execução dos comandos por parte do robot.

-}

module Main where

import Data.Char

{-| Definição do tipo 'Tabuleiro' que é uma lista de strings (com cada string correspondendo a uma linha).

-}

type Tabuleiro = [String]

{-| Definição do tipo 'Orientacao' que é um carater (iremos utilizar 'N','S','E e 'O').

-}
type Orientacao = Char

{-| Definição do tipo 'Posicao' que é um par de inteiros (abcissa e ordenada de uma posição qualquer).

-}

type Posicao = (Int,Int)

{-| Função 'outStr' que se encarrega apenas de moldar o input para ser trabalhado pelo progama (recebe o tabuleiro como sendo uma lista de string e aplica-lhe a função pré-definida 'unlines', dando como output uma só string com separadores do tipo /n).

-}

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

{-| Este pedaço de código serve para ler do ficheiro txt o conteúdo do jogo.

-}

main = do inp <- getContents
          putStr (outStr(tarefa5(lines inp)))

{-| A função 'tarefa5' é a função principal da tarefa. É dela que resulta o código html que irá servir para visualizar o jogo.

*@tab = takeWhile (all isALpha) txt@ - ao receber o tabuleiro completo retira as linhas que são letras (ou, por outra palavras, exclui a linha das posições (penúltima linha) e a dos comandos (que foi omitida para esta tarefa)).

*@(numx, numy, o) = stringParaTuplo (words (txt!!(length txt-1)))@ - é aplicada função pré-definida "words" à linha das posições criando uma lista de strings (divindindo as várias strings onde ouver um espaço) que já poderá ser usada pela função 'stringParaTuplo' que irá pegar nessa lista de strings e dar como output um tuplo com a coordenada x inicIal, a coordenada y inicial e a orientação inicial.

*@numLinhas = length tab@ - é aplicada a função pré-definida 'length' ao tabuleiro (que é uma lista de strings) para saber o número de linhas do mesmo (i.e. length da lista de strings).

*@altPosInicial = desenharRobotAux (procuraTab tab (numx,numy))@ - é chamada a função 'desenharRobotAux' com o 'procuraTab' da posição inicial (@(numx,numy)@) (de modo a que o programa saiba qual é o inteiro correspondente à altura da letra correspondente à posição inicial do robot (e.g. a=1, b=2, etc)). A 'altPosInicial' irá corresponder ao valor da altura da posição inicial do robot somado de uma unidade para que o robot não se encontre na posição do cubo superior mas sim um pouco mais acima.

*@txt = prefixo@ e @sufixo@ - são chamadas as funções 'prefixo' e 'sufixo' que são funções que apenas dão como output as partes do códigos necessárias à visualização do jogo mas que não sofrem alterações de tabuleiro para tabuleiro e que contêm informações como, por exemplo, a definição das formas usadas nos cubos. 

*@criaTabuleiro tab (numLinhas - 1)@ - Além disso, é chamada a função 'criaTabuleiro' que se encarrega de gerar o código para a visualização do tabuleiro. É chamada com input do tabuleiro e do número de linhas subtraído de uma unidade. 

*@desenharRobot (numx,(-1 * numy)) altPosInicial@ - e a função 'desenharRobot' que, por sua vez, se encarrega de gerar o código para a visualização do robot na sua posição inicial. É chamada com as coordenadas da posição inicial (com o inverso da ordenanda - ver observação) e com o valor calculdado em 'altPosInicial'.

OBS.: Ao testar o código vimos que a pagina html estava a mostrar o tabuleiro com orientação errada (i.e. a primeira linha do tabuleiro estava a aparecer na parte de trás da visualização) e, por isso, invertemos os valores da numLinhas multiplicando por -1 e já obtivemos resultados corretos.

-}          

tarefa5 :: [String] -> [String]
tarefa5 txt = prefixo ++ (criaTabuleiro tab (numLinhas - 1)) ++ desenharRobot (numx,(-1 * numy)) altPosInicial ++ sufixo -- ao testar o código vimos que a pagina html estava a mostrar o tabuleiro com orientação errada (i.e. a primeira linha do tabuleiro estava a aparecer na parte de trás da visualização) e, por isso, invertemos os valores da numLinhas multiplicando por -1 e já obtivemos resultador corretos 
    where tab = takeWhile (all isAlpha) txt
          (numx, numy, o) = stringParaTuplo (words (txt!!(length txt-1)))
          numLinhas = length tab
          altPosInicial = desenharRobotAux (procuraTab tab (numx,numy))

{-| A função 'stringParaTuplo' recebe com input uma lista de strings e dá como output um tuplo com a coordenada x inical, a coordenada y inical e a orientação inicial.

*@(read (s!!0)::Int, read (s!!1)::Int, head(s!!2))@ - recebendo a linha das posições do tabuleiro (e.g. "4 5 N"), lê (i.e. utiliza a função pré-definida "read") o elemento de índice 0 da string (o primeiro elemento da string; nesta caso, o número 4) e interpreta-o como um inteiro (para poder ser usado como tal posteriormente); lê o elemento de índice 1 (o segundo elemento da string; neste caso, o número 5) interpretando-o também como um inteiro; por fim, lê o elemento de índice 2 (o terceiro elemento da string, neste caso, a letra N), interpretando-o, por default, como um char.

-}

stringParaTuplo :: [String] -> (Int, Int, Orientacao)
stringParaTuplo s = (read (s!!0)::Int, read (s!!1)::Int, head(s!!2))

{-| A função 'procuraTab' recebe o tabuleiro e uma posição e dá como output a letra correspondente àquela posição.

-}

procuraTab :: Tabuleiro -> Posicao -> Char
procuraTab t (a,b) = (t!!((length t) - 1 - b)!!a)

{-| A função 'prefixo' apenas dá como output a parte do código html necessária à visualização do jogo mas que não sofre alterações de tabuleiro para tabuleiro e que está imediatamente antes do excerto do código de html que é gerado de forma a visualizar cada um dos tabuleiros específicos (i.e. função 'criaTabuleiro').

Contém:

*Uma parte inicial de abertura da página html e da componente do X3DOM que tornarão possível a visualizção da própria página html bem como a visualização das figuras, respetivamente.

*Uma parte final que contém uma cena (@<Scene>@) em que estão descritas as figuras que serão utilizadas para a visualização do tabuleiro e do robot (@cubo@,@cuboL@,@robot@).

-}

prefixo :: [String]
prefixo = [ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
          ,        "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
          , "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
          ,   "<head>"
          ,     "<meta http-equiv=\"X-UA-Compatible\" content=\"chrome=1\" />"
          ,     "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />"
          ,     "<title>Trabalho de LI de João Barreira e Sofia Carvalho</title>"
          ,     "<script src=\"http://www.x3dom.org/release/x3dom.js\"></script>"
          ,     "<link rel=\"stylesheet\" href=\"http://www.x3dom.org/release/x3dom.css\"/>"
          ,   "</head>"
          ,   "<body>"
          ,     "<h1>Visualização do Tabuleiro do Lightbot</h1>"
          ,     "<p class=\"case\">"
          ,       "<X3D xmlns=\"http://www.web3d.org/specifications/x3d-namespace\" id=\"boxes\""
          ,           "showStat=\"false\" showLog=\"false\" x=\"0px\" y=\"0px\" width=\"500px\" height=\"500px\">"
          ,         "<Scene>"
          ,           "<Shape DEF=\"cubo\">"
          ,             "<Appearance>"
          ,               "<Material diffuseColor='0 0 1.0'></Material>"
          ,             "</Appearance>"
          ,             "<Box size='.96 .96 .96'></Box>"
          ,           "</Shape>"
          ,           "<Shape DEF=\"cuboL\">"
          ,             "<Appearance>"
          ,               "<Material emissiveColor='0 0 1.0'></Material>"
          ,             "</Appearance>"
          ,             "<Box size='.96 .96 .96'></Box>"
          ,           "</Shape>"
          ,           "<Group render=\"false\">"
          ,             "<Shape DEF=\"robot\">"
          ,               "<Appearance>"
          ,                "<Material diffuseColor='0 .5 0'></Material>"
          ,               "</Appearance>"
          ,               "<Box size='.60 1 .60'></Box>"
          ,             "</Shape>"
          ,           "</Group>" ]

{-| A função 'prefixo' apenas dá como output a parte do código html necessária à visualização do jogo mas que não sofre alterações de tabuleiro para tabuleiro e que está imediatamente depois do excerto do código de html que é gerado de forma a visualizar cada um dos tabuleiros específicos (i.e. função 'criaTabuleiro'). Na função prefixo estão apenas presentes tags de fecho que servem para terminar o código html.

-}

sufixo :: [String]
sufixo = [        "</Scene>"
         ,      "</X3D>"
         ,    "</p>"
         ,    "<p> &nbsp; </p>"
         ,  "</body>"
         , "</html>" ]

{-| A função 'criaTabuleiro' utiliza a função 'criaLinhas' para gerar a parte do código html encarregue da visualização da totalidade um determinado tabuleiro (linhas e colunas), sendo dado como input o próprio tabuleiro bem como um Int correspondente ao número total de linhas do mesmo.

*@criaLinhas h 0 (-1 * numLinhas)@ - é chamada a função 'criaLinhas' com o primeira linha tabuleiro (@h@), o inteiro correspondente à coordenada x a 0 (para que a função 'criaLinhas' comece a trabalhar na posição de abcissa 0 e o inverso do inteiro correspondente ao número das linhas (para que a função 'criaLinhas' comece também na primeira linha; ver observação). Assim, a função 'criaLinhas' irá começar na posição (0,<numero de linhas do tabuleiro>) e, recursivamente, irá gerar o código necessário à visualização da totalidade primeira linha.

*@(criaTabuleiro t (numLinhas - 1))@ - depois a função 'criaTabuleiro' chama-se recursivamente, chamando por sua vez a função 'criaLinhas' para as restantes linhas do tabuleiro até que se chegue ao caso de paragem (@criaTabuleiro [] _ = []@ - o tubuleiro foi totalmente percorrido e encontra-se vazio).

OBS.: Ao testar o código vimos que a pagina html estava a mostrar o tabuleiro com orientação errada (i.e. a primeira linha do tabuleiro estava a aparecer na parte de trás da visualização) e, por isso, invertemos os valores da numLinhas multiplicando por -1 e já obtivemos resultados corretos.

-}

criaTabuleiro :: Tabuleiro -> Int -> [String]
criaTabuleiro [] _ = []
criaTabuleiro (h:t) numLinhas = (criaLinhas h 0 (-1 * numLinhas)) ++ (criaTabuleiro t (numLinhas - 1))

{-| A função 'criaLinhas' utiliza a função 'altura' para percorrer uma linha de um tabuleiro e gerar o código html necessário para a visualização dessa mesma linha.

*@(altura h x (ord (toLower h) - (ord 'a')) z)@ - chama a função 'altura' com o primeiro carater da linha (@h@), com a abcissa da posição (@x@), com a coordenada (@z@, que, no x3dom, corresponde à coordenada y das tarefa anteriores) e com o número correspondente à letra da posição do tabuleiro definido pela função @ord (toLower h) - (ord 'a')@ que subtrai à ord da letra em questão a ord do carater a, obtendo-se valores diferentes para cada uma das letras testadas (e.g. 'b'=1,'c'=2), sendo que estes valores correspondem ao nível da posição. Como output desta função intermédia irá resultar o código xhtml que servirá para colcar os cubos numa determinada posição até à altura da letra dessa mesma posição.

*@(criaLinhas t (x + 1) z)@ - depois, a função chama-se recursivamente, efetuando um processo análogo ao anterior, percorrendo a totalidade da linha até que se chegue ao fim da mesma (caso de paragem: @criaLinhas [] _ _ = []@).

-}

criaLinhas :: String -> Int -> Int -> [String]
criaLinhas [] _ _ = []
criaLinhas (h:t) x z = (altura h x (ord (toLower h) - (ord 'a')) z) ++ (criaLinhas t (x + 1) z)

{-| A função 'altura' recebe um carater correspondente a uma única posição do tabuleiro e vai colocando cubos até à altura dessa mesma posição (e.g. se o carater da posição (1,2) for um 'c' esta função colocará 3 cubos na posição (1,2) uns em cima dos outros). Além disso, testa também se esta mesma letra é maiúscula (i.e. se possui uma lâmpada e em caso afirmativo utiliza a shape "cuboL" em vez da "cubo"). 

*@altura ch x 0 z@ - primeiro está definido o caso da posição se encontrar na altura 0 (i.e. o caso da letra ser um 'a'; este caso funciona também como caso de paragem para a chamada recursiva da função mais abaixo descrita). A função testa se o carater da posição é maiúsculo (@isUpper ch@); em caso afirmativo, a função coloca um cubo com lâmpada (shape "cuboL") na posição (dada apenas na função principal), colocando a altura (coordenada y no x3dom) a 0; caso o carater não seja maiúsculo, a função efetuará um processo idêntico apenas usando a shape "cubo" em vez de "cuboL" (@<Shape USE=\"cubo\">@).

*@altura ch x h z@ - por fim, está definido o caso da posição se encontrar numa altura diferente de 0 (i.e. o caso da letra ser algo diferente de 'a'). A função começa por testar se o carater é maiúsculo (i.e. tem lâmpada) e usa a shape "cubo" ou "cuboL" de acordo com o resultado deste teste. Depois coloca um cubo na posição e com a altura correspondente à letra em questão (e.g. a=0, b=1, c=2, etc). Finalmente esta função é chamada recursivamente (@altura ch x (h - 1) z@) voltado a repetir o processo anterior até que a altura correspondente à letra da posição seja posta a 0 ou, por outras palavras, até que @(h-1)@ seja igual a 0 (voltando, assim, ao primeiro caso da função).

-}

altura :: Char -> Int -> Int -> Int -> [String]
altura ch x 0 z = if (isUpper ch)
                  then ["<Transform translation='" ++ (show x) ++ " 0 " ++ (show z) ++ "'> <Shape USE=\"cuboL\"> </Transform>"]
                  else ["<Transform translation='" ++ (show x) ++ " 0 " ++ (show z) ++ "'> <Shape USE=\"cubo\"> </Transform>"]
altura ch x h z = if (isUpper ch)
                  then ["<Transform translation='" ++ (show x) ++ " " ++ show h ++ " " ++ (show z) ++ "'> <Shape USE=\"cuboL\"> </Transform>"] ++ altura ch x (h - 1) z
                  else ["<Transform translation='" ++ (show x) ++ " " ++ show h ++ " " ++ (show z) ++ "'> <Shape USE=\"cubo\"> </Transform>"] ++ altura ch x (h - 1) z

{-| A função 'desenharRobot' recebe a posição inicial do robot (@(x,y)@) e um inteiro (@a@) (que posteriormente irá corresponder à altura da posição onde se encontra inicialmente) para gerar o código responsável pela visualização do próprio robot na posição inicial e em cima do cubo do tabuleiro correspondente a esta mesma posição.

*@show a@ - este valor irá corresponder na função principal à altura a que robot se encontrará (dado pela função 'desenharRobotAux').

*@<Shape USE=\"robot\">@ - para a visualização do robot é utilizada uma shape definida na função 'prefixo' chamada robot.

-}

desenharRobot :: Posicao -> Int -> [String]
desenharRobot (x,y) a = ["<Transform translation='" ++ (show x) ++ " " ++ (show a) ++ " " ++ (show y) ++ "'> <Shape USE=\"robot\"> </Transform>"]

{-| A função 'desenharRobotAux' irá dar à função 'desenharRobot' o valor da altura a que robot se encontrará. Para isso utiliza a diferença entre o ord do carater da posição em questão e o ord da letra 'a' somado de uma unidade para que o robot não se encontre na posição do cubo superior mas sim um pouco mais acima (@(ord (toLower ch) - (ord 'a')) + 1@).

*@(ord (toLower ch) - (ord 'a')) + 1@ - para isso, utilza 

-}

desenharRobotAux :: Char -> Int
desenharRobotAux ch =  (ord (toLower ch) - (ord 'a')) + 1