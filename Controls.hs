module Controls where 
import Graphics.Gloss.Interface.Pure.Game
import LI11819
import EstadosInicial
import Tarefa2_2018li1g101
-- | Funçao ativada quando há um evento No Gloss
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss (EventKey (Char 'm') Down _ _ ) estado = if gameState estado == InGame then estado {currentEstado = estadoInicial estado,gameState = Start} else estado
reageEventoGloss (EventKey (Char 'p') Down _ _ ) estado = if gameState estado == InGame then estado {gameState = InPause} else estado { gameState = InGame}
reageEventoGloss event@(EventKey key Up _ _) estado = if boolKey event then estado {teclas_press = retiraTeclaLista key (teclas_press estado)} else estado 
reageEventoGloss event@(EventKey key Down _ _) estado = if boolKey event && gameState estado == InGame then estado {teclas_press = [key]++(teclas_press estado)} else estado 
reageEventoGloss _ e = e
 
-- | Retira tecla da Lista quando a mesma é levantada
retiraTeclaLista :: Key -> [Key] -> [Key] 
retiraTeclaLista _ [] = []
retiraTeclaLista x (h:t) = if x==h then retiraTeclaLista x t else h:retiraTeclaLista x t

-- | Verifica se a tecla é para fazer mover o Jogador
boolKey :: Event -> Bool
boolKey (EventKey (SpecialKey key) _ _ _) = True
boolKey (EventKey (Char key) _ _ _ ) = True
boolKey _ = False



-- | Mapeamento das teclas do Jogo e a sua função
keyEvent :: Key -> Estado -> Estado
keyEvent (SpecialKey KeyUp) e = jogada 0 (Movimenta C) e
keyEvent (SpecialKey KeyDown) e = jogada 0 (Movimenta B) e
keyEvent (SpecialKey KeyLeft) e = jogada 0 (Movimenta E) e
keyEvent (SpecialKey KeyRight) e = jogada 0 (Movimenta D) e
keyEvent (Char ',') e = jogada 0 (Dispara Canhao) e
keyEvent (Char '.') e = jogada 0 (Dispara Laser) e
keyEvent (Char '-') e = jogada 0 (Dispara Choque) e
keyEvent (Char 'w' ) e = jogada 1 (Movimenta C) e
keyEvent (Char 's') e = jogada 1 (Movimenta B) e
keyEvent (Char 'a') e = jogada 1 (Movimenta E) e
keyEvent (Char 'd') e = jogada 1 (Movimenta D) e
keyEvent (Char '1') e = jogada 1 (Dispara Canhao) e
keyEvent (Char '2') e = jogada 1 (Dispara Laser) e
keyEvent (Char '3') e = jogada 1 (Dispara Choque) e
keyEvent (Char 't' ) e = jogada 2 (Movimenta C) e
keyEvent (Char 'g') e = jogada 2 (Movimenta B) e
keyEvent (Char 'f') e = jogada 2 (Movimenta E) e
keyEvent (Char 'h') e = jogada 2 (Movimenta D) e
keyEvent (Char '4') e = jogada 2 (Dispara Canhao) e
keyEvent (Char '5') e = jogada 2 (Dispara Laser) e
keyEvent (Char '6') e = jogada 2 (Dispara Choque) e
keyEvent (Char 'i' ) e = jogada 3 (Movimenta C) e
keyEvent (Char 'k') e = jogada 3 (Movimenta B) e
keyEvent (Char 'j') e = jogada 3 (Movimenta E) e
keyEvent (Char 'l') e = jogada 3 (Movimenta D) e
keyEvent (Char '7') e = jogada 3 (Dispara Canhao) e
keyEvent (Char '8') e = jogada 3 (Dispara Laser) e
keyEvent (Char '9') e = jogada 3 (Dispara Choque) e
keyEvent _ s = s

