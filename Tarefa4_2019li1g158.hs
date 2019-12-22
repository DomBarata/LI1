-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g158 where

import LI11920
import Tarefa0_2019li1g158
import Tarefa1_2019li1g158
import Tarefa2_2019li1g158

resistenciaAr = 0.125
accelGravidade = 1

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(2,mapaTeste, jogador1),(7.9, mapaTeste, jogador2),(0.1, mapaTeste, jogador3),(14.3,mapaTeste,jogador4),(5,mapaTeste,jogador3),(9,mapaTeste,jogador2),(13,mapaTeste,jogador1), (5, mapaTeste, (Jogador 1 3.2 7 0 (Ar 2 (-12) 0)))]

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
acelera t m j | estaChao  (estadoJogador j) = j {velocidadeJogador = aceleraChao j t m}
              | estaAr    (estadoJogador j) = aceleraAr j t
              | estaMorto (estadoJogador j) = j

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m j | estaMorto e = let tempo = timeoutJogador e - t
                           in if tempo <= 0 then j {velocidadeJogador = 0 , estadoJogador = (Chao False)}
                           else j {estadoJogador = (Morto tempo)}
           | estaChao e  = moveChao t m j
           | estaAr e    = moveAr t m j
   where e = (estadoJogador j)

-- | Altera o estado e a posição de um 'Jogador' que está no chão.
moveChao :: Double -- ^ O tempo decorrido.
         -> Mapa -- ^ O 'Mapa' utilizado.
         -> Jogador -- ^ O estado inicial do 'Jogador'.
         -> Jogador -- ^ O estado final do 'Jogador'.
moveChao t m j | truncate (distPercorrida) > truncate ((distanciaJogador j) + 1) && iPecaAtual <= iPecaDepois
                  = j {distanciaJogador = fromIntegral(truncate ((distanciaJogador j) + 1))}
               | truncate (distPercorrida) > truncate ((distanciaJogador j) + 1)
                  = j {distanciaJogador = fromIntegral(truncate ((distanciaJogador j) + 1)), estadoJogador = (Ar altura (getInclinacao pecaAtual) 0)}
               | otherwise              = j {distanciaJogador = distPercorrida}
    where dist           = t * velocidadeJogador j
          distPercorrida = dist + distanciaJogador j
          pecaAtual      = m!!(pistaJogador j)!!(truncate (distanciaJogador j))
          pecaDepois     = m!!(pistaJogador j)!!(truncate ((distanciaJogador j)+1))
          iPecaAtual     = getInclinacao pecaAtual
          iPecaDepois    = getInclinacao pecaDepois
          altura         = getAltura 1.0 pecaAtual

-- | Altera o estado e/ou a posição de um 'Jogador' que está no ar.
moveAr :: Double -- ^ O tempo decorrido.
       -> Mapa -- ^ O 'Mapa' utilizado.
       -> Jogador -- ^ O estado inicial do 'Jogador'.
       -> Jogador -- ^ O 'Estado' final do 'Jogador'.
moveAr t m j | x1 < limitePeca && y1 > getAltura x1 pecaAtual   --Jogador nao muda de peca, e nao toca na peca
               = arParaAr t j-- jogador fica no ar
             | x1 >= limitePeca && y2 > getAltura x2 pecaAtual -- jogador nao toca na peca
               = arParaAr' t j -- jogador fica no ar
             | abs (iJogador - iPecaAtual) >= 45 --jogador bate no chao com inclinaçao maior que 45
               = arParaMorto t m j -- morre fdp
             | otherwise --chega ao chao inteiro
               = arParaChao t m j -- posicao no chao
  where
    posAtualJogador = Cartesiano (distanciaJogador j) (alturaJogador (estadoJogador j))
    velocidadeNoJogador = somaVetores posAtualJogador (Polar ((velocidadeJogador j)*t) (inclinacaoJogador (estadoJogador j)))
    gravidadeNoJogador  = somaVetores posAtualJogador (Polar ((gravidadeJogador (estadoJogador j)) * t) (-90))
    posFinalJogador@(Cartesiano x1 y1) = somaVetores velocidadeNoJogador gravidadeNoJogador
    pecaAtual = m!!(pistaJogador j)!!(truncate (distanciaJogador j))
    limitePeca = (fromIntegral(truncate (distanciaJogador j))) + 1
    retaLimite = ((Cartesiano limitePeca 0),(Cartesiano limitePeca 1))
    retaTrajetoria = (posAtualJogador, posFinalJogador)
    novaPosFinalJogador@(Cartesiano x2 y2) = intersecao retaLimite retaTrajetoria
    iJogador = inclinacaoJogador (estadoJogador j)
    iPecaAtual = getInclinacao pecaAtual

arParaAr :: Double -> Jogador -> Jogador
arParaAr t j = j { distanciaJogador = x1, estadoJogador = (estadoJogador j) {alturaJogador = y1}}
  where
    posAtualJogador = Cartesiano (distanciaJogador j) (alturaJogador (estadoJogador j))
    velocidadeNoJogador = somaVetores posAtualJogador (Polar ((velocidadeJogador j)*t) (inclinacaoJogador (estadoJogador j)))
    gravidadeNoJogador  = somaVetores posAtualJogador (Polar ((gravidadeJogador (estadoJogador j)) * t) (-90))
    posFinalJogador@(Cartesiano x1 y1) = somaVetores velocidadeNoJogador gravidadeNoJogador

arParaAr' :: Double -> Jogador -> Jogador
arParaAr' t j = j { distanciaJogador = x1, estadoJogador = (estadoJogador j) {alturaJogador = y1}}
  where
    posAtualJogador = Cartesiano (distanciaJogador j) (alturaJogador (estadoJogador j))
    velocidadeNoJogador = somaVetores posAtualJogador (Polar ((velocidadeJogador j)*t) (inclinacaoJogador (estadoJogador j)))
    gravidadeNoJogador  = somaVetores posAtualJogador (Polar ((gravidadeJogador (estadoJogador j)) * t) (-90))
    posFinalJogador = somaVetores velocidadeNoJogador gravidadeNoJogador
    retaTrajetoria = (posAtualJogador, posFinalJogador)
    limitePeca = (fromIntegral(truncate (distanciaJogador j))) + 1
    retaLimite = ((Cartesiano limitePeca 0),(Cartesiano limitePeca 1))
    novaPosFinalJogador@(Cartesiano x1 y1) = intersecao retaLimite retaTrajetoria

arParaMorto :: Double -> Mapa -> Jogador -> Jogador
arParaMorto t m j = j { distanciaJogador = x1 , velocidadeJogador = 0 , estadoJogador = Morto 1 }
  where
    pecaAtual = m!!(pistaJogador j)!!(truncate (distanciaJogador j))
    retaPeca = defineReta pecaAtual j
    posAtualJogador = Cartesiano (distanciaJogador j) (alturaJogador (estadoJogador j))
    velocidadeNoJogador = somaVetores posAtualJogador (Polar ((velocidadeJogador j)*t) (inclinacaoJogador (estadoJogador j)))
    gravidadeNoJogador  = somaVetores posAtualJogador (Polar ((gravidadeJogador (estadoJogador j)) * t) (-90))
    posFinalJogador = somaVetores velocidadeNoJogador gravidadeNoJogador
    retaTrajetoria = (posAtualJogador, posFinalJogador)
    novaPosFinalJogador@(Cartesiano x1 y1) = intersecao retaPeca retaTrajetoria

arParaChao :: Double -> Mapa -> Jogador -> Jogador
arParaChao t m j = j { distanciaJogador = x1 , velocidadeJogador = velocidade, estadoJogador = Chao False }
  where
    pecaAtual = m!!(pistaJogador j)!!(truncate (distanciaJogador j))
    retaPeca = defineReta pecaAtual j
    posAtualJogador@(Cartesiano x0 y0) = Cartesiano (distanciaJogador j) (alturaJogador (estadoJogador j))
    velocidadeNoJogador = somaVetores posAtualJogador (Polar ((velocidadeJogador j)*t) (inclinacaoJogador (estadoJogador j)))
    gravidadeNoJogador  = somaVetores posAtualJogador (Polar ((gravidadeJogador (estadoJogador j)) * t) (-90))
    posFinalJogador = somaVetores velocidadeNoJogador gravidadeNoJogador
    retaTrajetoria = (posAtualJogador, posFinalJogador)
    novaPosFinalJogador@(Cartesiano x1 y1) = intersecao retaPeca retaTrajetoria
    velocidade = x1 - x0


defineReta :: Peca -> Jogador -> Reta
defineReta (Recta _ h) j = let posI = Cartesiano (fromIntegral(truncate (distanciaJogador j))) (fromIntegral h)
                               posF = Cartesiano (fromIntegral(truncate (distanciaJogador j))+1) (fromIntegral h)
                            in (posI, posF)
defineReta (Rampa _ i f) j = let posI = Cartesiano (fromIntegral(truncate (distanciaJogador j))) (fromIntegral i)
                                 posF = Cartesiano (fromIntegral(truncate (distanciaJogador j))+1) (fromIntegral f)
                              in (posI, posF)

-- | Devolve True se o 'Jogador' está 'Morto'.
estaMorto :: EstadoJogador -> Bool
estaMorto (Morto _) = True
estaMorto _         = False

-- | Devolve True se o 'Jogador' está no 'Ar'.
estaAr :: EstadoJogador -> Bool
estaAr (Ar _ _ _) = True
estaAr _          = False

-- | Devolve True se o 'Jogador' está no 'Chao'.
estaChao :: EstadoJogador -> Bool
estaChao (Chao _) = True
estaChao _        = False

-- | Altera a velocidade de um 'Jogador', se este se encontra no 'Chao'.
aceleraChao :: Jogador -- ^ O estado inicial do 'Jogador'.
            -> Double -- ^ O tempo decorrido.
            -> Mapa -- ^ O 'Mapa' utilizado.
            -> Double -- ^ A velocidade do 'Jogador' após acelerar.
aceleraChao j t m = let res = v + (accelMota - (atrito p) * v) * t
                    in if res < 0
                        then 0
                        else res
                    where v         = velocidadeJogador j
                          p         = getPiso (m!!(pistaJogador j)!!(truncate (distanciaJogador j)))
                          accelMota = if v<2 && aceleraJogador (estadoJogador j) then 1
                                                                                 else 0

-- | Devolve o valor do 'atrito' de um determinado 'Piso'.
atrito :: Piso -- ^ O 'Piso' da 'Peca' onde o 'Jogador' se encontra.
       -> Double -- ^ O valor do 'atrito' de acordo com esse 'Piso'.
atrito Terra = 0.25
atrito Relva = 0.75
atrito Lama = 1.50
atrito Boost = (-0.50)
atrito Cola = 3.00

aceleraAr :: Jogador -- ^ O estado inicial do 'Jogador'.
          -> Double -- ^ O tempo decorrido.
          -> Jogador -- ^
aceleraAr j t = let res = (v - (resistenciaAr * v * t))
                in if res < 0
                    then j {velocidadeJogador = 0,
                            estadoJogador     = (estadoJogador j) {gravidadeJogador = (g + accelGravidade * t)}}
                    else j {velocidadeJogador = (v - (resistenciaAr * v * t)),
                            estadoJogador     = (estadoJogador j) {gravidadeJogador = (g + accelGravidade * t)}}
                where v                       = velocidadeJogador j
                      g                       = gravidadeJogador (estadoJogador j)
