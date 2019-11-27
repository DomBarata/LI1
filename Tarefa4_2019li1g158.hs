-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g158 where

import LI11920
import Tarefa0_2019li1g158
import Tarefa2_2019li1g158

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = []

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera = undefined

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m j | estaMorto e = let tempo = timeoutJogador e - t
                           in if tempo <= 0 then j {estadoJogador = (Chao False)}
                           else j {estadoJogador = (Morto tempo)}
           | estaChao e  = moveChao t m j
           | estaAr e    = moveAr t m j
   where e = (estadoJogador j)

moveChao :: Double -> Mapa -> Jogador -> Jogador
moveChao t m j | truncate (distPercorrida) > truncate (dist)
                                           = if iPecaAtual <= iPecaDepois
                  then j {distanciaJogador = fromIntegral (truncate (dist+1))}
                  else j {distanciaJogador = fromIntegral (truncate (dist+1)), estadoJogador = (Ar altura (getInclinacao pecaAtual) 0)}
               | otherwise                 = j {distanciaJogador = distPercorrida}
    where dist           = t * velocidadeJogador j
          distPercorrida = dist + distanciaJogador j
          pecaAtual      = m!!(pistaJogador j)!!(truncate (distanciaJogador j))
          pecaDepois     = m!!(pistaJogador j)!!(truncate ((distanciaJogador j)+1))
          iPecaAtual     = getInclinacao pecaAtual
          iPecaDepois    = getInclinacao pecaDepois
          altura         = getAltura 1.0 pecaAtual

moveAr :: Double -> Mapa -> Jogador -> Jogador
moveAr t m j =
  where dist = t * velocidadeJogador j
        posJogador = ((distanciaJogador j), (alturaJogador (estadoJogador j)))

estaMorto :: EstadoJogador -> Bool
estaMorto (Morto _) = True
estaMorto _         = False

estaAr :: EstadoJogador -> Bool
estaAr (Ar _ _ _) = True
estaAr _          = False

estaChao :: EstadoJogador -> Bool
estaChao (Chao _) = True
estaChao _        = False
