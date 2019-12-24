-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

--import LI11920
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

data Imagens = Imagens{
               retaTerra    :: Picture,
               retaRelva    :: Picture,
               retaLama     :: Picture,
               retaBoost    :: Picture,
               retaCola     :: Picture,
               rampaTerra01 :: Picture,
               rampaTerra02 :: Picture,
               rampaTerra10 :: Picture,
               rampaTerra20 :: Picture,
               rampaTerra30 :: Picture,
               rampaTerra40 :: Picture,
               rampaRelva01 :: Picture,
               rampaRelva02 :: Picture,
               rampaRelva10 :: Picture,
               rampaRelva20 :: Picture,
               rampaRelva30 :: Picture,
               rampaRelva40 :: Picture,
               rampaLama01  :: Picture,
               rampaLama02  :: Picture,
               rampaLama10  :: Picture,
               rampaLama20  :: Picture,
               rampaLama30  :: Picture,
               rampaLama40  :: Picture,
               rampaBoost01 :: Picture,
               rampaBoost02 :: Picture,
               rampaBoost10 :: Picture,
               rampaBoost20 :: Picture,
               rampaBoost30 :: Picture,
               rampaBoost40 :: Picture,
               rampaCola01  :: Picture,
               rampaCola02  :: Picture,
               rampaCola10  :: Picture,
               rampaCola20  :: Picture,
               rampaCola30  :: Picture,
               rampaCola40  :: Picture }

loadPictures :: IO()
loadPictures = do retaTerra    <- loadJuicy "resources/retaTerra.png"
                  retaRelva    <- loadJuicy "resources/retaRelva.png"
                  retaLama     <- loadJuicy "resources/retaLama.png"
                  retaBoost    <- loadJuicy "resources/retaBoost.png"
                  retaCola     <- loadJuicy "resources/retaCola.png"
                  rampaTerra01 <- loadJuicy "resources/rampaTerra01.png"
                  rampaTerra02 <- loadJuicy "resources/rampaTerra02.png"
                  rampaTerra10 <- loadJuicy "resources/rampaTerra10.png"
                  rampaTerra20 <- loadJuicy "resources/rampaTerra20.png"
                  rampaTerra30 <- loadJuicy "resources/rampaTerra30.png"
                  rampaTerra40 <- loadJuicy "resources/rampaTerra40.png"
                  rampaRelva01 <- loadJuicy "resources/rampaRelva01.png"
                  rampaRelva02 <- loadJuicy "resources/rampaRelva02.png"
                  rampaRelva10 <- loadJuicy "resources/rampaRelva10.png"
                  rampaRelva20 <- loadJuicy "resources/rampaRelva20.png"
                  rampaRelva30 <- loadJuicy "resources/rampaRelva30.png"
                  rampaRelva40 <- loadJuicy "resources/rampaRelva40.png"
                  rampaLama01  <- loadJuicy "resources/rampaLama01.png"
                  rampaLama02  <- loadJuicy "resources/rampaLama02.png"
                  rampaLama10  <- loadJuicy "resources/rampaLama10.png"
                  rampaLama20  <- loadJuicy "resources/rampaLama20.png"
                  rampaLama30  <- loadJuicy "resources/rampaLama30.png"
                  rampaLama40  <- loadJuicy "resources/rampaLama40.png"
                  rampaBoost01 <- loadJuicy "resources/rampaBoost01.png"
                  rampaBoost02 <- loadJuicy "resources/rampaBoost02.png"
                  rampaBoost10 <- loadJuicy "resources/rampaBoost10.png"
                  rampaBoost20 <- loadJuicy "resources/rampaBoost20.png"
                  rampaBoost30 <- loadJuicy "resources/rampaBoost30.png"
                  rampaBoost40 <- loadJuicy "resources/rampaBoost40.png"
                  rampaCola01  <- loadJuicy "resources/rampaCola01.png"
                  rampaCola02  <- loadJuicy "resources/rampaCola02.png"
                  rampaCola10  <- loadJuicy "resources/rampaCola10.png"
                  rampaCola20  <- loadJuicy "resources/rampaCola20.png"
                  rampaCola30  <- loadJuicy "resources/rampaCola30.png"
                  rampaCola40  <- loadJuicy "resources/rampaCola40.png"

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
main :: IO ()
main = play window          -- janela onde irá correr o jogo
            background      -- côr do fundo da janela
            fr              -- frame rate
            estadoGlossInicial   -- estado inicial
            desenhaEstado   -- desenha o estado do jogo
            reageEvento     -- reage a um evento
            reageTempo      -- reage ao passar do tempo

window :: Display
window = InWindow "Excite Bike" (400, 400) (0, 0)

background :: Color
background = greyN 0.8

fr :: Int
fr = 60

type EstadoGloss = (Estado, [Picture])

estadoGlossInicial :: [Picture] -> EstadoGloss
estadoGlossInicial l = (Estado m js,lp)

desenhaEstado :: Estado -> Picture
desenhaEstado (x,y) = Translate x y poligno
  where
    poligno :: Picture
    poligno = Polygon [(0,0),(10,0),(10,10),(0,10),(0,0)]

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) (x,y) = (x,y+5)
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) (x,y) = (x,y-5)
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (x,y) = (x-5,y)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (x,y) = (x+5,y)
reageEvento _ s = s -- ignora qualquer outro evento

reageTempo :: Float -> Estado -> Estado
reageTempo n (x,y) = (x,y-0.1)

desenhaPista :: [Peca] -> Float -> Float -> [Picture]
desenhaPista [] _ _ = []
desenhaPista (h:t) x y = (desenhaPeca h x y) : (desenhaPista t (x+1) y)

desenhaMapa :: Mapa -> Float -> Float -> [Picture]
desenhaMapa [] _ _ = []
desenhaMapa (h:t) x y = (desenhaPista h x y) ++ (desenhaMapa t x (y+1))

desenhaPeca :: Peca -> Float -> Float -> Picture
desenhaPeca (Recta Terra i)   x y = Translate x y retaTerra
desenhaPeca (Recta Relva i)   x y = Translate x y retaRelva
desenhaPeca (Recta Lama  i)   x y = Translate x y retaLama
desenhaPeca (Recta Boost i)   x y = Translate x y retaBoost
desenhaPeca (Recta Cola  i)   x y = Translate x y retaCola
desenhaPeca (Rampa Terra i f) x y = undefined
desenhaPeca (Rampa Relva i f) x y = undefined
desenhaPeca (Rampa Lama  i f) x y = undefined
desenhaPeca (Rampa Boost i f) x y = undefined
