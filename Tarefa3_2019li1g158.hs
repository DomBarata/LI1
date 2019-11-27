-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g158 where

import LI11920

-- Testes

p1 = [Recta Terra 0, Recta Terra 0, Recta Boost 0, Recta Boost 0, Recta Boost 0, Recta Lama 0]

p2 = [Recta Terra 0, Recta Terra 0, Recta Terra 0, Recta Terra 0, Rampa Lama 0 2, Rampa Lama 2 0]

p3 = [Recta Terra 0, Recta Terra 0,Recta Terra 0, Rampa Boost 0 1, Rampa Lama 1 0, Rampa Boost 0 1, Rampa Lama 1 0,Recta Terra 0,Recta Terra 0]

p4 = [Recta Terra 0,Rampa Boost 0 1, Rampa Lama 1 0, Rampa Boost 0 1, Rampa Lama 1 0,Recta Terra 0,Recta Terra 0, Recta Terra 0, Recta Terra 0]

-- Mapas para testes

mapa1 =[[Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Rampa Relva 0 2,Rampa Relva 2 3],
        [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 1],
        [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Boost 0],
        [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Boost 0]]

mapa2 = [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],
         [Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]]

mapa3 = [[Recta Terra 0,Recta Terra 0,Recta Boost 0],
         [Recta Terra 0,Recta Terra 0,Recta Relva 0]]

mapa4 = [[Recta Terra 0,Recta Relva 0,Recta Relva 0]]

mapa5 = [[Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Lama 0],
         [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Rampa Boost 0 2,Rampa Terra 2 4,Recta Lama 4,Rampa Relva 4 3,Recta Relva 3],
         [Recta Terra 0,Recta Relva 0,Recta Boost 0,Rampa Relva 0 2,Recta Relva 2,Recta Terra 2,Recta Terra 2,Recta Boost 2,Rampa Boost 2 0,Recta Boost 0],
         [Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Relva 0,Recta Terra 0,Rampa Boost 0 1,Recta Lama 1,Rampa Relva 1 0,Recta Terra 0,Rampa Terra 0 1],
         [Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Lama 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Boost 0]]

mapa6 = [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Terra 0,Rampa Relva 0 2,Rampa Relva 2 4,Recta Relva 4,Recta Relva 4,Rampa Relva 4 0,Recta Lama 0],
         [Recta Terra 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 4,Recta Terra 4,Rampa Terra 4 5,Rampa Terra 5 6,Rampa Terra 6 5,Recta Terra 5],
         [Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Lama 0,Rampa Lama 0 2,Recta Lama 2,Rampa Lama 2 0,Recta Terra 0],
         [Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Rampa Terra 1 3,Rampa Lama 3 2,Recta Lama 2,Recta Terra 2,Rampa Terra 2 1,Rampa Boost 1 0],
         [Recta Terra 0,Recta Lama 0,Rampa Lama 0 2,Rampa Lama 2 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 2],
         [Recta Terra 0,Rampa Lama 0 2,Recta Lama 2,Rampa Lama 2 3,Rampa Terra 3 1,Rampa Terra 1 3,Recta Terra 3,Recta Terra 3,Recta Terra 3,Recta Boost 3],
         [Recta Terra 0,Rampa Relva 0 2,Rampa Terra 2 3,Rampa Terra 3 0,Recta Terra 0,Recta Lama 0,Rampa Lama 0 1,Rampa Lama 1 0,Recta Lama 0,Recta Lama 0],
         [Recta Terra 0,Recta Lama 0,Recta Lama 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Boost 0,Recta Terra 0],
         [Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 3,Rampa Boost 3 1,Recta Boost 1,Rampa Terra 1 2,Rampa Terra 2 0,Recta Terra 0,Recta Boost 0],
         [Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0]]

mapa7 = [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Boost 0,Recta Boost 0,Rampa Boost 0 2],
         [Recta Terra 0,Recta Terra 0,Rampa Relva 0 2,Recta Boost 2,Rampa Boost 2 3,Rampa Terra 3 1,Recta Terra 1],
         [Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Recta Relva 1,Rampa Relva 1 0,Rampa Relva 0 1,Recta Terra 1],
         [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Rampa Relva 0 2,Recta Terra 2]]

m0 = [p3, p4]

m1 = [p1,p2]


-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [m0, m1, mapa1, mapa2, mapa3, mapa4, mapa5, mapa6, mapa7]


-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
desconstroi :: Mapa -> Instrucoes
desconstroi m = desconstroiHorizontal 1 (matrizParaLista (mapaParaInstrucoes m 0))

-- | Encontra padrões horizontais numa dada lista de 'Instrucao'
desconstroiHorizontal :: Int -- ^ Elemento auxiliar à chamada recursiva, que indica o número de 'Peca' a repetir (começa a 1)
                      -> Instrucoes -- ^ Lista de Instrucao' a desconstruir
                      -> Instrucoes -- ^ Lista desconstruída
desconstroiHorizontal _ [] = []
desconstroiHorizontal n (h1:h2:t) | h1 == h2          = desconstroiHorizontal (n+1) (h2:t)
                                  | h1 /= h2 && n > 1 = Repete n [h1]:desconstroiHorizontal 1 (h2:t)
                                  | otherwise         = h1:desconstroiHorizontal 1 (h2:t)
desconstroiHorizontal n [h1]      | n > 1             = Repete n [h1]:[]
                                  | otherwise         = h1:[]

-- | Desconstroi um 'Mapa' em 'Instrucoes' sem nenhuma otimização
desconstroiBasico :: Mapa -> Instrucoes
desconstroiBasico m = matrizParaLista (mapaParaInstrucoes m 0)

-- | Dada uma lista de 'Instrucoes', concatena tudo numa só lista
matrizParaLista :: [Instrucoes] -> Instrucoes
matrizParaLista (h:t)  = h ++ matrizParaLista t
matrizParaLista _     = []

-- | Dada um 'Mapa' e o índice de uma pista, devolve as respetivas 'Instrucoes'
mapaParaInstrucoes :: Mapa -> Int -> [Instrucoes] -- mapa + Pista
mapaParaInstrucoes l n | length l > n = (drop 1 (map (pecaParaInstrucao n) (l!!n))) : mapaParaInstrucoes l (n+1)
                       | otherwise    = []

-- | Dado o índice da Pista e a respetíva 'Peca', devolve a 'Instrucao'
pecaParaInstrucao :: Int -> Peca -> Instrucao
pecaParaInstrucao i (Recta piso _)                   = Anda [i] piso
pecaParaInstrucao i (Rampa piso altInicial altFinal) | altInicial < altFinal = Sobe [i] piso (altFinal-altInicial)
                                                     | otherwise             = Desce [i] piso (altInicial-altFinal)
