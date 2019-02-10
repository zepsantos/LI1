module Jogador where 
import Debug.Trace      
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Controls
import LI11819
import Tarefa2_2018li1g101
import Tarefa0_2018li1g101


moveJogador :: [Key] -> Estado -> Estado
moveJogador [] e = e
moveJogador (h:t) e  = keyEvent (h) $ moveJogador t e


mkJogador :: Estado -> Picture -> Picture
mkJogador e tank = pictures(map (mkTank tank) jog )
                where jog = jogadoresVivos (jogadoresEstado e)
                
jogadoresVivos :: [Jogador] -> [Jogador]  
jogadoresVivos [] = []
jogadoresVivos (h:t) = if vidasJogador h > 0 then h:jogadoresVivos t else jogadoresVivos t             

mkDisparos :: Estado -> Picture -> Picture -> Picture
mkDisparos e disp l = pictures(map (mkDisparo disp l e) (disparosEstado e))


mkDisparo :: Picture -> Picture -> Estado -> Disparo  -> Picture
mkDisparo c l e d | tipoDisparo  d == Canhao = trace (show pos) Translate x y (imagemNaDirecao c)    -- FALTA EXPANDIR O LASER PARA 2.5 SE O LASER FOR PARA CIMA OU PARA BAIXO
                  | tipoDisparo  d == Laser  = pictures (map (\ (a,b) -> Translate a b (scale 2 1 (imagemNaDirecao l)) ) laserParaReferential)
                  | otherwise = Translate x y (imagemNaDirecao c)
                where (x,y) = toReferentialBala dir pos
                      dir = direcaoDisparo d
                      pos = posicaoDisparo d
                      imagemNaDirecao img = rodaImagem dir img
                      laserParaReferential = map (toReferentialBala dir) (laserPos d e)
                      
laserPos :: Disparo -> Estado -> [Posicao]
laserPos d e = if encontraPosicaoMatriz pos (mapaEstado e) /= Bloco Indestrutivel then pos:laserPos disp e else []
            where pos = posicaoDisparo d                      
                  disp = d{posicaoDisparo = somaVetores (posicaoDisparo d) (direcaoParaVetor (direcaoDisparo d))}
tipoDisparo :: Disparo -> Arma
tipoDisparo DisparoCanhao {jogadorDisparo = _ , posicaoDisparo = _ , direcaoDisparo = _} = Canhao                      
tipoDisparo DisparoLaser {jogadorDisparo = _ , posicaoDisparo = _ , direcaoDisparo = _} = Laser                      
tipoDisparo DisparoChoque {jogadorDisparo = _ , tempoDisparo = _} = Choque                      

mkTank :: Picture -> Jogador -> Picture
mkTank tank jog = Translate x y (rodaImagem dir tank)
                  where (x,y) = toReferentialTank pos
                        dir = direcaoJogador jog
                        pos = posicaoJogador jog



-- | Roda a imagem do tanque consoante a direção para onde está virado
rodaImagem :: Direcao -> Picture -> Picture
rodaImagem D p= rotate 270.0 p
rodaImagem B p= rotate 0.0 p
rodaImagem C p= rotate 180.0 p
rodaImagem E p= rotate 90.0 p

-- | Mete a posicao do Tank do Jogador numa posicao do referencial do Gloss
toReferentialTank :: Posicao -> (Float,Float)
toReferentialTank (x,y) = (realToFrac (posInicialX +((y)*tamanhoBloco)),realToFrac (posInicialY-((x)*tamanhoBloco)))

toReferentialBala :: Direcao -> Posicao -> (Float,Float)
toReferentialBala C (x,y)  = (realToFrac (posInicialX+25+((y-1)*tamanhoBloco)),realToFrac (posInicialY-13-((x)*tamanhoBloco)))
toReferentialBala D (x,y)  = (realToFrac (posInicialX+15+((y-1)*tamanhoBloco)),realToFrac (posInicialY-25-((x-1)*tamanhoBloco)))
toReferentialBala B (x,y)  = (realToFrac (posInicialX+25+((y-1)*tamanhoBloco)),realToFrac (posInicialY-15-((x-1)*tamanhoBloco)))
toReferentialBala E (x,y)  = (realToFrac (posInicialX+15+((y)*tamanhoBloco)),realToFrac (posInicialY-25-((x-1)*tamanhoBloco)))
