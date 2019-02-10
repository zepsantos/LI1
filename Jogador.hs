module Jogador where 
import Debug.Trace      
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Controls
import LI11819
import Debug.Trace
import Tarefa2_2018li1g101
import Tarefa0_2018li1g101
import Tarefa6_2018li1g101
import Data.Maybe

-- | Funcao que move o Jogador baseado nas teclas pressionadas
moveJogador :: [Key] -> Estado -> Estado
moveJogador [] e = e
moveJogador (h:t) e  = keyEvent h $ moveJogador t e

-- | Faz a jogada do Bot
jogadaBot :: Int -> Estado -> Estado
jogadaBot n e = if isJust botJogada then (jogada n (fromJust botJogada) e) else e
        where botJogada = bot n e

-- | Funcao que desenha os jogadores no jogo
mkJogador :: Estado -> Picture -> Picture
mkJogador e tank = pictures(map (mkTank tank) jog )
                where jog = jogadoresVivos (jogadoresEstado e)
         
-- | Funcao que retorna uma lista dos jogadores que estao vivos                
jogadoresVivos :: [Jogador] -> [Jogador]  
jogadoresVivos [] = []
jogadoresVivos (h:t) = if vidasJogador h > 0 then h:jogadoresVivos t else jogadoresVivos t             

mkDisparos :: Estado -> Picture -> Picture -> Picture
mkDisparos e disp l = pictures(map (mkDisparo disp l e) (disparosEstado e))

-- | Funcao que desenha um disparo no jogo para construir uma lista de todos os disparos
mkDisparo :: Picture -> Picture -> Estado -> Disparo  -> Picture
mkDisparo c l e d | tipoDisparo  d == Canhao = Translate x y (imagemNaDirecao c)
                  | tipoDisparo  d == Laser  = pictures (map (\ (a,b) -> Translate a b (laserImage dir) ) laserParaReferential)
                  | tipoDisparo  d == Choque = desenhaChoque d e
                  | otherwise = blank
                where (x,y) = toReferentialBala dir pos
                      dir = direcaoDisparo d
                      pos = posicaoDisparo d
                      laserImage f = if f == D || f == E then scale 3.5 1 (imagemNaDirecao l) else scale 1 3.5 (imagemNaDirecao l)
                      imagemNaDirecao img = rodaImagem dir img
                      laserParaReferential = map (toReferentialLaser dir) (laserPos d e)
                      
laserPos :: Disparo -> Estado -> [Posicao]
laserPos d e = if  isVazia pos && isVazia pos1 then pos:laserPos disp e else []
            where (pos,pos1) = getPosLaser (direcaoDisparo d) (posicaoDisparo d)
                  isVazia pos = encontraPosicaoMatriz pos (mapaEstado e) /= Bloco Indestrutivel
                  disp = d{posicaoDisparo = somaVetores (posicaoDisparo d) (direcaoParaVetor (direcaoDisparo d))}
                  
desenhaChoque :: Disparo -> Estado -> Picture
desenhaChoque d e = Color blue $ Line [toReferentialTank (x+3,y-3), toReferentialTank (x+3,y+3) ,toReferentialTank(x-3,y+3),toReferentialTank (x-3,y-3),toReferentialTank (x+3,y-3)]
                where jog = encontraIndiceLista (jogadorDisparo d) (jogadoresEstado e)
                      (x,y) = posicaoJogador jog                  
-- | Retorna ambas as Posicoes do Disparo
getPosLaser :: Direcao -> Posicao -> (Posicao,Posicao)
getPosLaser C (x,y) = ((x,y),(x,y+1))
getPosLaser B (x,y) = ((x+1,y),(x+1,y+1))
getPosLaser D (x,y) = ((x,y+1),(x+1,y+1))
getPosLaser E (x,y) = ((x,y),(x+1,y))                  

tipoDisparo :: Disparo -> Arma
tipoDisparo DisparoCanhao {jogadorDisparo = _ , posicaoDisparo = _ , direcaoDisparo = _} = Canhao                      
tipoDisparo DisparoLaser {jogadorDisparo = _ , posicaoDisparo = _ , direcaoDisparo = _} = Laser                      
tipoDisparo DisparoChoque {jogadorDisparo = _ , tempoDisparo = _} = Choque                      

-- | Funcao que desenha o tank do jogador no jogo na posição onde ele se encontrar                      
mkTank :: Picture -> Jogador -> Picture
mkTank tank jog = Translate x y (rodaImagem dir tank)
                  where (x,y) = toReferentialTank pos
                        dir = direcaoJogador jog
                        pos = posicaoJogador jog



-- | Roda a imagem do tanque consoante a direção para onde está virado
rodaImagem :: Direcao -> Picture -> Picture
rodaImagem D p= rotate 270.0 (Color red p)
rodaImagem B p= rotate 0.0 (Color red p)
rodaImagem C p= rotate 180.0 (Color red p)
rodaImagem E p= rotate 90.0 (Color red p)

-- | Mete a posicao do Tank do Jogador numa posicao do referencial do Gloss
toReferentialTank :: Posicao -> (Float,Float)
toReferentialTank (x,y) = (realToFrac (posInicialX +(y*tamanhoBloco)),realToFrac (posInicialY-(x*tamanhoBloco)))

toReferentialBala :: Direcao -> Posicao -> (Float,Float)
toReferentialBala C (x,y)  = (realToFrac (posInicialX+(y*tamanhoBloco)),realToFrac (posInicialY-10-(x*tamanhoBloco)))
toReferentialBala D (x,y)  = (realToFrac (posInicialX-10 +(y*tamanhoBloco)),realToFrac (posInicialY-(x*tamanhoBloco)))
toReferentialBala B (x,y)  = (realToFrac (posInicialX+(y*tamanhoBloco)),realToFrac (posInicialY+10-(x*tamanhoBloco)))
toReferentialBala E (x,y)  = (realToFrac (posInicialX+10+(y*tamanhoBloco)),realToFrac (posInicialY-(x*tamanhoBloco)))

toReferentialLaser :: Direcao -> Posicao -> (Float,Float)
toReferentialLaser C (x,y)  = (realToFrac (posInicialX+(y*tamanhoBloco)),realToFrac (posInicialY-(x*tamanhoBloco)))
toReferentialLaser D (x,y)  = (realToFrac (posInicialX-25+(y*tamanhoBloco)),realToFrac (posInicialY-(x*tamanhoBloco)))
toReferentialLaser B (x,y)  = (realToFrac (posInicialX+(y*tamanhoBloco)),realToFrac (posInicialY+25-(x*tamanhoBloco)))
toReferentialLaser E (x,y)  = (realToFrac (posInicialX+(y*tamanhoBloco)),realToFrac (posInicialY-(x*tamanhoBloco)))
