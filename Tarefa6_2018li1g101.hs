-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2018li1g101 where
import Tarefa4_2018li1g101
import Tarefa0_2018li1g101
import Tarefa2_2018li1g101
import LI11819
import Debug.Trace
import Data.Maybe

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n e = trace (show jog) jog
      where jog = moveOBot n e $ destroiParedesParaPontos n e $ atacaOsJogadores n e $ defendeDosTiros Nothing n e

destroiParedesParaPontos :: Int -> Estado -> Maybe Jogada -> Maybe Jogada
destroiParedesParaPontos _ _ (Just j) = Just j
destroiParedesParaPontos n e Nothing = Nothing

 {-
      1. Move o Bot para ao pe dos inimigos usando a nearbyPlayers
 -}
moveOBot :: Int -> Estado -> Maybe Jogada -> Maybe Jogada
moveOBot _ _ (Just j) = Just j
moveOBot n e Nothing = Nothing

{-
      1. ENCONTRAR JOGADORES PROXIMOS PARA ANDAR PARA O PE DOS MESMOS
      2. Usando distanciaEntreDoisPontos?  
-}
nearbyPlayer :: Jogador -> Estado -> Maybe Jogador
nearbyPlayer jog e = undefined --map (distanciaEntrePontos (posicaoJogador jog)) $ map (posicaoJogador) (jogadoresEstado e)   

 {-
  1. -- SE ESTIVER A MENOS DE X DE DISTANCIA DISPARAR SE FOR ACERTAR , USAR FUNCAO vaiAcertar
  2. Usar Choque se estiver no range e se TIVER
  3. Usar laser se estiver mais do que um tank alinhado? Ou por tras de muitas caixas (tipo 4?)
  4. 
 -}

atacaOsJogadores :: Int -> Estado -> Maybe Jogada -> Maybe Jogada
atacaOsJogadores _ _ (Just j) = Just j
atacaOsJogadores n e Nothing = Nothing 
-- | Defende dos tiros, tentando destruir os canhoes com canhoes ou lasers
atacaTiros ::  Int -> Estado -> Maybe Jogada -> Maybe Jogada
atacaTiros  _ _ (Just j) = Just j
atacaTiros  n e Nothing = if isJust closeShot then shotToDefend n e $ fromJust closeShot else Nothing
                        where closeShot = tirosNasProximidades e n (disparosEstado e)
                              jog = encontraIndiceLista n (jogadoresEstado e)         
                              

shotToDefend :: Int -> Estado -> Disparo -> Maybe Jogada                              
shotToDefend n e d  = if direcaoJogador jog == dirOpostas (direcaoDisparo d) then (if jogadorEmFrenteAoOutro (n,jog) e d && lasersJogador jog > 0 then Just $ Dispara Laser else Just $ Dispara Canhao) else Just $ Movimenta $ dirOpostas (direcaoDisparo d) 
                  where jog = encontraIndiceLista n (jogadoresEstado e)
jogadorEmFrenteAoOutro :: (Int,Jogador) -> Estado -> Disparo -> Bool
jogadorEmFrenteAoOutro (n,jog) e d = vaiAcertar (DisparoLaser n  dispPos (dirOpostas $ direcaoDisparo d)) jogAAcertar e 
                              where dispPos = somaVetores (posicaoJogador jog) (direcaoParaVetor (direcaoJogador jog))
                                    jogAAcertar = encontraIndiceLista (jogadorDisparo d) (jogadoresEstado e)
-- * Defende dos Tiros

-- | Defende dos tiros
defendeDosTiros :: Maybe Jogada -> Int -> Estado -> Maybe Jogada
defendeDosTiros (Just j) _ _  = Just j
defendeDosTiros j n e = atacaTiros n e $ defendeTiros n e j
   

-- | Defende dos tiros , tentando mover o jogador
defendeTiros ::  Int -> Estado -> Maybe Jogada -> Maybe Jogada
defendeTiros  _ _ (Just j) = Just j
defendeTiros  n e Nothing = if isJust closeShot then tryMove (fromJust closeShot) n e else Nothing
                        where closeShot = tirosNasProximidades e n (disparosEstado e)         
                              jog = encontraIndiceLista n (jogadoresEstado e)
               
tryMove :: Disparo -> Int -> Estado -> Maybe Jogada
tryMove d n e | podeMoverNaDirecaoDoJogador (direcaoJogador jog) (direcaoDisparo d)  && veSePodeMover jog (direcaoJogador jog) e = Just (Movimenta $ direcaoJogador jog)
              | veSePodeMover jog (posicaoParaDesvio (direcaoDisparo d)) e = Just (Movimenta (posicaoParaDesvio (direcaoDisparo d)))
              | veSePodeMover jog (dirOpostas $ posicaoParaDesvio (direcaoDisparo d)) e = Just (Movimenta (dirOpostas $ posicaoParaDesvio (direcaoDisparo d)))
              | otherwise = Nothing
               where jog = encontraIndiceLista n (jogadoresEstado e)

veSePodeMover :: Jogador -> Direcao -> Estado -> Bool
veSePodeMover j d e = verificaMoverValido pos j e 
                  where pos = somaVetores  (posicaoJogador j) (direcaoParaVetor d)

podeMoverNaDirecaoDoJogador:: Direcao -> Direcao -> Bool
podeMoverNaDirecaoDoJogador C B = False                  
podeMoverNaDirecaoDoJogador B C = False                  
podeMoverNaDirecaoDoJogador D E = False                  
podeMoverNaDirecaoDoJogador E D = False                  
podeMoverNaDirecaoDoJogador _ _  = True

posicaoParaDesvio :: Direcao -> Direcao
posicaoParaDesvio B = D
posicaoParaDesvio E = B
posicaoParaDesvio D = B
posicaoParaDesvio C = D                  
               {-
tryMove :: (Int,Bool) -> Int -> Estado  -> Maybe Jogada                             
tryMove jogInfo indJog e | dIsVertical (direcaoJogador jog) == (not $ snd jogInfo) &&  veSePodeMover jog (direcaoJogador jog) e = Just  (Movimenta $ direcaoJogador jog)
                         | veSePodeMover jog (dirOpostas (fst jogInfo, not $ snd jogInfo)) e = Just $ returnTipoJogada jogInfo 
                         | fst jogInfo == 0 = tryMove (1,snd jogInfo) indJog e 
                         | otherwise = Nothing
                        where jog = encontraIndiceLista indJog (jogadoresEstado e)



returnTipoJogada :: (Int,Bool) -> Jogada
returnTipoJogada info = Movimenta (dirOpostas info)
-}
-- | Da me a direçao oposta de onde vem o disparo e o inteiro e usado para saber se ja tentou virar para algum lado                               
dirOpostas :: Direcao -> Direcao
dirOpostas E = D -- Se o disparo e vertical
dirOpostas D = E -- Se nao puder mover para a direita
dirOpostas C = B -- Se o disparo e horizontal
dirOpostas B = C -- Se nao puder mover para Baixo            

-- | Verifica se o disparo é vertical
dIsVertical :: Direcao -> Bool
dIsVertical C = True
dIsVertical B = True
dIsVertical _ = False




-- | Devolve me um tiro proximo se existir  // TODO : MUDAR A MANEIRA COMO O PROXIMO SE ENCONTRA. A DISTANCIA TEM DE SER 
tirosNasProximidades :: Estado -> Int -> [Disparo] -> Maybe Disparo
tirosNasProximidades _ _ [] = Nothing
tirosNasProximidades e n (h@DisparoCanhao {jogadorDisparo = jD, posicaoDisparo = pD , direcaoDisparo = dir}:t) = if jD /= n && vaiAcertar h jog e && distanciaEntrePontos pD (posicaoJogador jog) <= 3  then Just h else tirosNasProximidades e n t
                                                                                                            where tankBox = tirosProxTank (posicaoJogador jog) 8
                                                                                                                  jog = encontraIndiceLista n (jogadoresEstado e)
tirosNasProximidades e n (h:t) = tirosNasProximidades e n t
                                                                                                            --tirosNasProximidades p (h@DisparoChoque{jogadorDisparo = jD,tickDisparo = _}:t) = tirosNasProximidades p t CASO QUEIRA FAZER ALGO SE HOUVER UM CHOQUE AO PE

vaiAcertar :: Disparo -> Jogador -> Estado -> Bool
vaiAcertar d j e = vaiAcertarJogador (dIsVertical $ direcaoDisparo d) posD posJ && not (noCaminhoBlocoIndestrutivel posJ (direcaoDisparo d) (posD) e )
            where posD = getPosDisp (direcaoDisparo d ) (posicaoDisparo d)
                  posJ = pos4Tank $ posicaoJogador j      

noCaminhoBlocoIndestrutivel :: [Posicao] -> Direcao -> (Posicao,Posicao) -> Estado -> Bool
noCaminhoBlocoIndestrutivel [] _ _ _ = False
noCaminhoBlocoIndestrutivel l dir (pos,pos1) e = if peca pos /= Bloco Indestrutivel || peca pos1 /= Bloco Indestrutivel then (if aBater l (pos,pos1) then False else noCaminhoBlocoIndestrutivel l dir newPos e) else True
                                                where peca a = encontraPosicaoMatriz a (mapaEstado e)
                                                      newPos = (somaVetores pos (direcaoParaVetor dir),somaVetores pos1 (direcaoParaVetor dir))
                                                      
aBater :: [Posicao] -> (Posicao,Posicao) -> Bool
aBater [] _ = False
aBater (h:t) (pos,pos1) = h == pos || h == pos1 || aBater t (pos,pos1)

vaiAcertarJogador :: Bool ->  (Posicao,Posicao) -> [Posicao] -> Bool
vaiAcertarJogador _ _ [] = False    
vaiAcertarJogador False posD@((x,y),(x1,y1)) ((a,b):t) =  x == a && y /= b || x1 == a && y1 /= b || vaiAcertarJogador False posD t    
vaiAcertarJogador True  posD@((x,y),(x1,y1)) ((a,b):t) =  x /= a && y == b || x1 /= a && y1 == b || vaiAcertarJogador True posD t



tirosProxTank :: Posicao -> Int -> [Posicao]
tirosProxTank _ 0 = []
tirosProxTank (x,y) n = (x+n,y):(x,y+n):(x-n,y):(x,y-n):tirosProxTank (x,y) (n-1) 

-- | Devolve o minimo inteiro de uma lista
minimoIntDaLista :: [Int] -> Int -> Int
minimoIntDaLista [] n = n
minimoIntDaLista (h:t) n = minimoIntDaLista t (min n h)

-- | Retorna a distancia entre os dois pontos , util para saber se o disparo esta perto
distanciaEntrePontos :: Posicao -> Posicao -> Int
distanciaEntrePontos (x,y) (x1,y1) = round $ sqrt $ fromIntegral(x1-x)^2 + fromIntegral(y1-y)^2 
                                    
-- | Devolve as 4 posicoes relativas ao tank do jogador na posicao
pos4Tank :: Posicao -> [Posicao]
pos4Tank (x,y) = [(x,y),(x+1,y),(x+1,y+1),(x,y+1)]