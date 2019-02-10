module TickRun where
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI11819
import Tarefa4_2018li1g101
import Jogador
import Data.Maybe

--
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss n e = if gameState e == InGame then e { currentEstado =  jogaBot e $  moveJogador  (teclas_press e) $ tick (currentEstado e), timer = (timer e) +1 } else e
 {-if (timer e) `mod` fr == 0 then e { currentEstado =  moveJogador  (teclas_press e) $ tick (currentEstado e), timer = (timer e) +1 } else e{timer = (timer e) +1}
                    where time = (timer e) `div` fr-}

jogaBot :: EstadoGloss -> Estado -> Estado 
jogaBot eG e = if isJust (botIsActive eG) then jogadaBot (fromJust (botIsActive eG)) e else e           
            
