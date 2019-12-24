{- |

= Introdução
Nsta tarefa tivemos que criar um 'bot', ou seja, um jogador robot, que fosse capaz de jogar sem nenhuma interação do utilizador.

= Objetivos


= Discussão e Conclusão


-}

-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g158 where

import LI11920
import Tarefa0_2019li1g158
import Tarefa1_2019li1g158
import Tarefa2_2019li1g158
import Tarefa4_2019li1g158

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot i estado@(Estado mapa js) | vBot == 0 || distBot < (length pistaBot) = Just Acelera
                              | pecaMaisRapida distBot pistaBot mapa == "Frente" = Just (Movimenta D)
                              | pecaMaisRapida distBot pistaBot mapa == "Cima" = Just (Movimenta C)
                              | pecaMaisRapida distBot pistaBot mapa == "Baixo" = Just (Movimenta B)
                              | colaBot>0 && jogadorAtras pistaBot distBot l = Just Dispara
                              | pecaSeguinteCola pistaBot distBot mapa = Just Desacelera
                              | otherwise = Nothing
                              where (Jogador pistaBot distBot vBot colaBot eBot) = js!!i
                                    (as,b:bs) = splitAt i js
                                    l = as++bs

-- | Devolve uma 'String' de acordo com a 'Peca' adjacente ao 'bot' com menor valor de atrito.
pecaMaisRapida :: Double -- ^ A distância do 'bot' em relação ao ínicio da pista.
               -> Int -- ^ O número da 'Pista' onde o 'bot' se encontra.
               -> Mapa -- ^ O 'Mapa' usado.
               -> String -- ^ 'String' que identifica a 'Peca' com menor atrito ("Cima" - 'Peca' da 'Pista' anterior, "Baixo" - 'Peca' da 'Pista' seguinte e "Frente" - 'Peca' seguinte da 'Pista' atual).
pecaMaisRapida dist pista m | eIndiceListaValido (pista-1) m && podeMover dist pecaAtual pecaCima
                              && atrito (getPiso pecaCima) < atrito (getPiso pecaBaixo) && atrito (getPiso pecaCima) < atrito (getPiso pecaFrente) = "Cima"
                            | eIndiceListaValido (pista+1) m && podeMover dist pecaAtual pecaBaixo
                             && atrito (getPiso pecaBaixo) < atrito (getPiso pecaCima) && atrito (getPiso pecaBaixo) < atrito (getPiso pecaFrente) = "Baixo"
                            | otherwise = "Frente"
                            where pecaCima = m!!(pista-1)!!(truncate dist)
                                  pecaBaixo = m!!(pista+1)!!(truncate dist)
                                  pecaFrente = m!!pista!!((truncate dist)+1)
                                  pecaAtual = m!!pista!!(truncate dist)

-- | Devolve um 'Bool' que nos diz se é possível mover o 'bot' para outra 'Pista'.
podeMover :: Double -- ^ A distância do 'bot' em relação ao ínicio da 'Pista'.
          -> Peca -- ^ A 'Peca' atual do 'bot'.
          -> Peca -- ^ A 'Peca' para a qual o 'bot' se vai mover.
          -> Bool -- ^ 'Bool' que indica se o 'bot' pode mover-se para essa 'Peca' ou não.
podeMover dist pecaAtual pecaSeguinte | (getAltura dist pecaAtual)-(getAltura dist pecaSeguinte)<=0.2 && (getAltura dist pecaAtual)-(getAltura dist pecaSeguinte)>=(-0.2) = True
                                      | (getAltura dist pecaAtual)>(getAltura dist pecaSeguinte) = True
                                      | otherwise = False

-- | Identifica a presença de algum 'Jogador' na 'Peca' anterior àquela em que o 'bot' se encontra.
jogadorAtras :: Int -- ^ A 'Pista' onde se encontra o 'bot'.
             -> Double -- ^ A distância do 'bot' em relação ao ínicio da 'Pista'.
             -> [Jogador] -- ^ Lista de 'Jogador'es do jogo sem o 'bot'.
             -> Bool -- ^ 'Bool' que indica se existe algum 'Jogador' na 'Peca' anterior áquela onde o 'bot' se encontra.
jogadorAtras _ _ []     = False
jogadorAtras pista dist (h:t) | pistaJogador h == pista && distanciaJogador h < fromIntegral(truncate dist) && distanciaJogador h >= fromIntegral((truncate dist)-1) = True
                              | otherwise = jogadorAtras pista dist t

-- | Indica se a 'Peca' seguinte àquela em que se encontra o 'bot' tem 'Cola'.
pecaSeguinteCola :: Int -- ^ A 'Pista' onde se encontra o 'bot'.
                 -> Double -- ^ A distância do 'bot' em relação ao ínicio da 'Pista'.
                 -> Mapa -- ^ O 'Mapa' usado.
pecaSeguinteCola pista dist m = (getPiso peca) == Cola
                        where peca = m!!pista!!((truncate dist)+1)
