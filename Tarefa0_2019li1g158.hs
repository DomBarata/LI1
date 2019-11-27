-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g158 where

-- * Funções não-recursivas.

-- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo deriving (Show)

-- | Um ângulo em graus.
type Angulo = Double

-- ** Funções sobre vetores

-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** Funções gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x1 + x2) (y1 + y2)
somaVetores v1 v2                                 = somaVetores (polar2cart v1) (polar2cart v2)

-- | Converte um vetor polar para um vetor cartesiano
polar2cart :: Vetor -> Vetor
polar2cart (Polar r a)        = Cartesiano (r * cos (a*pi/180)) (r * sin (a*pi/180))
polar2cart c@(Cartesiano _ _) = c

-- | Converte um ponto polar para um ponto cartesiano
polar2cart' :: Ponto -> Ponto
polar2cart' (Polar r a)        = Cartesiano (r * cos (a*pi/180)) (r * sin (a*pi/180))
polar2cart' c@(Cartesiano _ _) = c

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x1 - x2) (y1 - y2)
subtraiVetores v1 v2                                 = subtraiVetores (polar2cart v1) (polar2cart v2)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor n (Cartesiano x y) = Cartesiano (n * x) (n * y)
multiplicaVetor n v                = multiplicaVetor n (polar2cart v)

-- ** Funções sobre rectas.

-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto,Ponto)
-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersetam :: Reta -> Reta -> Bool
intersetam r1 r2 = let ta = tA r1 r2
                       tb = tB r1 r2
                   in ta >= 0 && ta <= 1 && tb >= 0 && tb <= 1
                   where tA :: Reta -> Reta -> Double
                         tA ((Cartesiano x1 y1), (Cartesiano x2 y2)) ((Cartesiano x3 y3), (Cartesiano x4 y4)) = ((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3)) / ((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
                         tA (p1,p2) (p3,p4)                                                                   = tA (polar2cart' p1, polar2cart' p2) (polar2cart' p3, polar2cart' p4)
                         tB :: Reta -> Reta -> Double
                         tB ((Cartesiano x1 y1), (Cartesiano x2 y2)) ((Cartesiano x3 y3), (Cartesiano x4 y4)) = ((y1-y2)*(x1-x3)+(x2-x1)*(y1-y3)) / ((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
                         tB (p1,p2) (p3,p4)                                                                   = tB (polar2cart' p1, polar2cart' p2) (polar2cart' p3, polar2cart' p4)

-- | Calcular o segemento de interseção entre duas retas
segmentoIntersecao :: Reta -> Reta -> Ponto
segmentoIntersecao ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) =
                   (Cartesiano (((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3))/((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3)))
                   (((y1-y2)*(x1-x3)+(x2-x1)*(y1-y3)/(x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))))
segmentoIntersecao (p1,p2) (p3,p4)                                                                 = segmentoIntersecao (polar2cart p1, polar2cart p2) (polar2cart p3, polar2cart p4)

-- | Calcular o declive de uma reta
declive :: Ponto -> Ponto -> Double
declive (Cartesiano x1 y1) (Cartesiano x2 y2) = (x1-x2)/(y1-y2)
declive p1 p2                                 = declive (polar2cart p1) (polar2cart p2)

-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao (p1,p2) (p3,p4) = somaPontos p1 (multiplicaPonto t1 (subtraiPontos p2 p1))
  where (Cartesiano t1 _) = segmentoIntersecao (p1,p2)(p3,p4)

-- | Soma dois 'Ponto's.
somaPontos :: Ponto -> Ponto -> Ponto
somaPontos (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x1 + x2) (y1 + y2)
somaPontos v1 v2                                 = somaPontos (polar2cart v1) (polar2cart v2)

-- | Subtrai dois 'Ponto's.
subtraiPontos :: Ponto -> Ponto -> Ponto
subtraiPontos (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x1 - x2) (y1 - y2)
subtraiPontos v1 v2                                 = subtraiPontos (polar2cart v1) (polar2cart v2)

-- | Multiplica um escalar por um 'Ponto'.
multiplicaPonto :: Double -> Ponto -> Ponto
multiplicaPonto n (Cartesiano x y) = Cartesiano (n * x) (n * y)
multiplicaPonto n v                = multiplicaPonto n (polar2cart v)


-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
--
-- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido i l = i >= 0 && length l > i

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
--
-- __Sugestão:__ relembre a função 'length', referida anteriormente.
dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz [] = (0,0)
dimensaoMatriz (l:ls) | length l == 0 = (0,0)
                      | otherwise     = (1+length ls, length l)

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool
ePosicaoMatrizValida _ []      = False
ePosicaoMatrizValida _ [[]]    = False
ePosicaoMatrizValida (p1,p2) m = fst (dimensaoMatriz m) > p1 && snd (dimensaoMatriz m) > p2

-- * Funções recursivas.

-- ** Funções sobre ângulos

-- | Normaliza um ângulo na gama [0..360).
--  Um ângulo pode ser usado para representar a rotação
--  que um objecto efectua. Normalizar um ângulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientação do
--  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo x |x < 0     = normalizaAngulo (x+360)
                  |x >= 360  = normalizaAngulo (x-360)
                  |otherwise = x

-- ** Funções sobre listas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista i (h:t) | i < 0 || i >= length (h:t) = h
                            | i == 0                     = h
                            | otherwise                  = encontraIndiceLista (i-1) t


-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista i x (h:t) | i < 0 || i >= length (h:t) = (h:t)
                              | i == 0                     = (x:t)
                              | otherwise                  = h:atualizaIndiceLista (i-1) x t

-- ** Funções sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz p ((l:ls):t) | p < (0,0) ||
                                     fst p >= fst(dimensaoMatriz ((l:ls):t)) ||
                                     snd p >= snd(dimensaoMatriz ((l:ls):t)) = l
                                   | p == (0,0)                              = l
                                   | fst p > 0                               = encontraPosicaoMatriz (fst p-1,snd p) t
                                   | snd p > 0                               = encontraPosicaoMatriz (fst p,snd p-1) (ls:t)

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz p x ((l:ls):t) | p < (0,0) ||
                                       fst (p) >= fst (dimensaoMatriz ((l:ls):t)) ||
                                       snd (p) >= snd (dimensaoMatriz ((l:ls):t)) = ((l:ls):t)
                                     | fst (p) > 0                                = (l:ls):atualizaPosicaoMatriz (fst p-1,snd p) x t
                                     | otherwise                                  = (atualizaIndiceLista (snd p) x (l:ls)):t
