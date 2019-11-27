-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g158 where

import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(0,5,1), {-(2,0,1),-} (2,5,1), (4,7,6), (5,10,3), (1,3,0), {-(0,0,0),-} {-(-1,-2,-5),-} (2,3,-1)]

--TESTES DE DESENVOLVIMENTO
--v :: [Int]
--v = [5,6,5,7,8,7,4,5,0,8,1,5,4,1,6,3]

-- | Todos os mapas começam com a peça __Recta Terra 0__
pecaInicial :: Peca
pecaInicial = Recta Terra 0

-- * Funções pré-definidas da Tarefa 1.

-- | Função fornecida pelos docentes, dado um número n e uma 'semente', gera uma lista de n números aleatórios
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.
-- | Função que dado um número n de pistas, um número de peças por pista e uma sementa, gera um mapa
gera :: Int -> Int -> Int -> Mapa
gera npistas comprimento semente = let necessario = 2*npistas*comprimento-2*npistas
                                       listaAleat = geraAleatorios necessario semente
                                       pares = criaListaDePares listaAleat
                                       pseudoMapa = divideListaEmMatriz npistas pares
                                   in  geraMapa pseudoMapa

-- | Dada uma matriz de pares inteiros, constrói um mapa
geraMapa :: [[(Int, Int)]] -> Mapa
geraMapa (h:t) = geraPista h pecaInicial:geraMapa t
geraMapa _     = []

-- | Recebe um numero de 0 a 9, a peça anterior e devolve o piso referente a esse número
descobrePiso :: Int -> Peca -> Piso
descobrePiso i p | i == 0 || i == 1 = Terra
                 | i == 2 || i == 3 = Relva
                 | i == 4           = Lama
                 | i == 5           = Boost
                 | otherwise        = getPiso p

-- | Dada uma lista, devolve uma lista de pares com os elementos sucessivos da lista dada
criaListaDePares :: [Int] ->[(Int, Int)]
criaListaDePares (h1:h2:t) = (h1,h2):criaListaDePares t
criaListaDePares _         = []

-- | Dados um número n e uma lista de pares, devolve uma matriz dividindo a lista em n listas
divideListaEmMatriz :: Int -> [(Int,Int)] -> [[(Int, Int)]]
divideListaEmMatriz 0 _  = []
divideListaEmMatriz _ [] = []
divideListaEmMatriz n l  = let tam   =  length l `div` n
                               (x,y) = splitAt tam l
                           in x:(divideListaEmMatriz (n-1) y)


-- | Dada uma peça, devolve a altura final da mesma
getAltura :: Peca -> Int
getAltura (Recta _ x)   = x
getAltura (Rampa _ _ x) = x

-- | Dada uma peça, devole o piso da mesma
getPiso :: Peca -> Piso
getPiso (Recta p _)   = p
getPiso (Rampa p _ _) = p

-- | Dado um par e a peça anterior, devolve a peça correspondente as caraterísticas do par indicado
getPeca :: (Int, Int) -> Peca -> Peca
getPeca (x, y) p | y == 0 || y == 1 = Rampa (descobrePiso x p) (getAltura p) ((getAltura p)+(y+1))
                 | y >= 2 && y <= 5 && getAltura p == 0 = Recta (descobrePiso x p) 0
                 | y >= 2 && y <= 5 && getAltura p /= 0 = let h = getAltura p - (y-1)
                                                          in if h < 0 then Rampa (descobrePiso x p) (getAltura p) 0
                                                             else Rampa (descobrePiso x p) (getAltura p) h
                 | otherwise                            = Recta (descobrePiso x p) (getAltura p)

-- | Dada uma lista de pares e a peca inicial, devolde a pista correspondente
geraPista :: [(Int,Int)] -> Peca -> Pista
geraPista [] x = [x]
geraPista (h:t) p = p:geraPista t p1
         where p1 = getPeca h p
