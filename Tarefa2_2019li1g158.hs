-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g158 where

import LI11920
import Tarefa0_2019li1g158

-- Testes

-- Jogadores para teste

jogador1 = Jogador 0 3.2 3 0 (Chao True)

jogador2 = Jogador 1 3.6 4 5 (Chao False)

jogador3 = Jogador 0 3.6 7 0 (Ar 3 23 0)

jogador4 = Jogador 0 3.6 0 2 (Morto 1)

-- Lista de jogadores para testes

jogs = [jogador1, jogador2]

jogs2 = [jogador3, jogador2]

jogs3 = [jogador4, jogador2]

-- Mapa de testes
-- É usado sempre o mesmo mapa

mapaTeste = [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],
             [Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]]

-- Estados para testes

e = Estado mapaTeste jogs

e2 = Estado mapaTeste jogs2

e3 = Estado mapaTeste jogs3

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0, Movimenta B, e), --  MORTO
            (1, Movimenta C, e), --  AR
            (0, Movimenta C, e), --  NADA
            (0, Desacelera, e),  --  Desacelera
            (1, Acelera, e),     --  Acelera
            (0, Movimenta B, e2),--  NADA
            (0, Movimenta E, e2),--  INCLINA
            (0, Movimenta D, e2),--  INCLINA
            (0, Dispara, e2),    --  NAO TEM COLA/ESTA NO AR
            (1, Dispara, e),     --  DISPARA
            (0, Dispara, e),     --  NAO TEM COLA
            (0, Movimenta D, e), --  NADA
            (0, Movimenta E, e), --  NADA
            (0, Movimenta C, e2),--  NADA
            (0, Movimenta B, e2),--  NADA
            (0, Movimenta D, e3),--  NADA
            (0, Movimenta E, e3),--  NADA
            (0, Movimenta C, e3),--  NADA
            (0, Movimenta B, e3),--  NADA
            (0, Acelera, e3),    --  NADA
            (0, Desacelera, e3), --  NADA
            (0, Dispara, e3),    --  NADA
            (1, Dispara, (Estado ([[Recta Terra 0, Rampa Relva 0 3, Recta Boost 3],[Recta Terra 0, Rampa Boost 0 4, Rampa Cola 4 8]])
            ([(Jogador 0 1 4 6 (Chao True)), (Jogador 1 2 5 6 (Chao True))])))]


-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada nj (Movimenta dir) (Estado m js) | eIndiceListaValido nj js
                                          && dir == C || dir == B = (Estado m (atualizaIndiceLista nj (mudaDePista jogador dir m) js))
                                        | eIndiceListaValido nj js
                                          && dir == D || dir == E = (Estado m (atualizaIndiceLista nj (inclinaJogador jogador dir) js))
                                        | otherwise = (Estado m js)
                                        where jogador = js!!nj
jogada nj jog (Estado m js)             | eIndiceListaValido nj js
                                          && jog == Acelera || jog == Desacelera = (Estado m (atualizaIndiceLista nj (aceleracaoJogador jogador jog) js))
                                        |eIndiceListaValido nj js
                                          && jog == Dispara = (Estado mapaAtualizado (atualizaIndiceLista nj jogadorAtualizado js))
                                        | otherwise = (Estado m js)
                                        where jogador = js!!nj
                                              mapaAtualizado = snd (temColaDispara jogador m)
                                              jogadorAtualizado = fst (temColaDispara jogador m)

-- | Dispara apenas se tiver cola e devolve resultado da funcao disparaCola
temColaDispara :: Jogador -> Mapa -> (Jogador,Mapa)
temColaDispara (Jogador p d v c est) m | estaNoChao (Jogador p d v c est)
                                       && (c-1)>=0                     = ((Jogador p d v (c-1) est) , (disparaCola (Jogador p d v (c-1) est) m))
                                     | otherwise                       = ((Jogador p d v c est), m)

-- | Dada a pista onde o jogador esta e a peca anterior, devolve o mapa com a pista com cola
disparaCola :: Jogador -> Mapa -> Mapa
disparaCola (Jogador p d v c est) m | estaNoChao (Jogador p d v c est)
                                    && d >= 1 = let pos            = (p, ((truncate d)-1))
                                                    peca           = m!!(fst pos)!!(snd pos)
                                                    pecaPeganhenta = pisoParaCola peca
                                                in  atualizaPosicaoMatriz pos pecaPeganhenta m
                                  | otherwise = m

-- | Altera o piso do mapa para o qual foi disparada cola
temCola :: Jogador -> Bool
temCola (Jogador _ _ _ c _) = c /= 0

-- | Altera o piso de uma dada peça para __Cola__
pisoParaCola :: Peca -> Peca
pisoParaCola (Recta _ x)   = Recta Cola x
pisoParaCola (Rampa _ x y) = Rampa Cola x y

-- | Altera a pista no estado de um determinado jogador de acordo com a direcao de movimento
mudaDePista :: Jogador -> Direcao -> Mapa -> Jogador
mudaDePista j dir m | estaNoChao j
                      && eIndiceListaValido ((getNumPista j)-1) m
                      && dir == C = moverJogador j (-1) m
                    | estaNoChao j
                      && eIndiceListaValido ((getNumPista j)+1) m
                      && dir == B = moverJogador j 1 m
                    | otherwise = j

-- | Altera a inclinacao do jogador no seu estado, no caso de estar no ar
inclinaJogador :: Jogador -> Direcao -> Jogador
inclinaJogador (Jogador p dist v c (Ar h i g)) E | i+15 <= 90 = Jogador p dist v c (Ar h (i+15) g)
                                                 | otherwise   =  Jogador p dist v c (Ar h 90 g)
inclinaJogador (Jogador p dist v c (Ar h i g)) D | i-15 >= -90  = Jogador p dist v c (Ar h (i-15) g)
                                                 | otherwise   =  Jogador p dist v c (Ar h (-90) g)
inclinaJogador j _                               = j

-- | Altera a aceleração do jogador
aceleracaoJogador :: Jogador -> Jogada -> Jogador
aceleracaoJogador (Jogador p dist v c (Chao False)) Acelera   = Jogador p dist v c (Chao True)
aceleracaoJogador (Jogador p dist v c (Chao True)) Desacelera = Jogador p dist v c (Chao False)
aceleracaoJogador j _ = j

-- | Determina se um jogador esta no chão ou não
estaNoChao :: Jogador -> Bool
estaNoChao (Jogador _ _ _ _ est) = (est == Chao True || est == Chao False)

-- | Devolve o número da pista onde o jogador se encontra
getNumPista :: Jogador -> Int
getNumPista (Jogador p _ _ _ _) = p

-- | Dado um jogador, um inteiro n e um mapa, move o jogador para a pista n
moverJogador :: Jogador -> Int -> Mapa -> Jogador
moverJogador (Jogador p dist v c est) dir m = let altAnt  = getAltura (dist - fromIntegral(truncate dist)) (m!!p!!(truncate dist))
                                                  alt     = getAltura (dist - fromIntegral(truncate dist)) (m!!(p+dir)!!(truncate dist))
                                                  pecaAnt = m!!p!!(truncate dist)
                                              in  if (altAnt-alt) <= 0.2 && (altAnt-alt) >= (-0.2)
                                                  then Jogador (p+dir) dist v c est
                                                  else if altAnt > alt
                                                       then Jogador (p+dir) dist v c (Ar altAnt (getInclinacao pecaAnt) 0)
                                                       else Jogador p dist 0 c (Morto 1)

-- | Dada uma peça, devolve a inclinação da mesma
getInclinacao :: Peca -> Double
getInclinacao (Recta _ _)   = 0
getInclinacao (Rampa _ x y) = let arg      = ((fromIntegral y)-(fromIntegral x))
                                  toDegres = 180/pi
                              in atan(arg)*toDegres

-- | Dada a posição de um jogador e uma peça, devolve a altura a que ele se encontra
getAltura :: Double -> Peca -> Double
getAltura _ (Recta _ x) = fromIntegral x
getAltura pos p         = tan(getInclinacao p / (180/pi))*pos
