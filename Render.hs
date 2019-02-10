module Render where 
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Debug.Trace
import LI11819
import Mapa
import Menu
import Jogador
import Controls
import Tarefa1_2018li1g101


-- | Estado Inicial
estadoGlossInicial :: Estado -> Picture -> Picture -> EstadoGloss
estadoGlossInicial estadoInicial bI f = EstadoGloss estadoInicial estadoInicial (mkMapa estadoInicial bI f) Start (Just 1) [] 0
                    

-- | Desenha o Estado na Picture para o Gloss representar
desenhaEstadoGloss :: Picture -> Picture -> Picture -> Picture -> Picture -> EstadoGloss  -> Picture
desenhaEstadoGloss tank disp laser pD pI eG  = desenhaGameState (gameState eG) (tank,disp,laser,pD,pI) eG

-- | Desenha a picture relativa ao estado de Jogo em que está
desenhaGameState :: GameStatus -> (Picture,Picture,Picture,Picture,Picture) -> EstadoGloss -> Picture
desenhaGameState Start _ _ = menuInicio
desenhaGameState InPause _ _ = menuPausa
desenhaGameState InGame (tank,disp,laser,pD,pI) eG =  pictures [pic,mkWallDest e pD,mkJogador e tank,mkDisparos e disp laser, mkinfo e]
                                        where pic = imagemGloss eG
                                              e = currentEstado eG