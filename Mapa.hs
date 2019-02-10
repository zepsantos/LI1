module Mapa where
import Debug.Trace
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import LI11819
import Tarefa1_2018li1g101
import Tarefa0_2018li1g101


-- | Desenha o Mapa
mkMapa :: Estado -> Picture -> Picture -> Picture
mkMapa estadoInicial bI f = pictures ([fundo] ++  vazia ++ [mkWallIndest estadoInicial bI] ++ [bordas] ++ [mkinfo estadoInicial] ) 
        where pos = obtemMapa (mapaEstado estadoInicial) (Bloco Indestrutivel) (0,0)
              posVazia = obtemMapa (mapaInicial (dimensaoMatriz (mapaEstado estadoInicial))) (Vazia) (1,0)
              bordas = mkLinha estadoInicial
              fundo = mkfundo f
              vazia = map mkVazia posVazia
              
            
              
--mkMapa :: Estado -> Picture -> Picture -> Picture
--mkMapa estadoInicial _ _ = pictures (mklinhalist (mklinha estadoInicial))

-- | Desenha as Paredes Indestrutiveis
mkWall :: Picture -> (Int,Int) -> Picture
mkWall bI (x,y) =  Translate x1 y1 bI
            where (x1,y1) = toReferential  (x,y)

-- | Desenha a parte vazia do Mapa lol     
mkVazia :: (Int,Int) -> Picture
mkVazia (x,y) = translate (xRef-13) (yRef+12) polygon --Translate xRef yRef polygon
          where polygon =  Color (greyN 1) $ Polygon (rectanglePath 25 25)
                (xRef,yRef) = toReferential  (x,y)

-- | Desenha as Paredes Destrutiveis atraves de um map das posições onde há um bloco destrutivel
mkWallDest :: Estado -> Picture -> Picture
mkWallDest e pD = pictures(map (mkDestrutivel pD) lpos)
               where lpos = obtemMapa (mapaEstado e) (Bloco Destrutivel) (1,0)

-- | Desenha a Parede Destrutivel 
mkDestrutivel :: Picture -> (Int,Int) -> Picture
mkDestrutivel pD (x,y)  = translate (x1-12.5) (y1+12.5) pD
                    where (x1,y1) = toReferential  (x,y)
                    

mkIndestrutivel :: Picture -> (Int,Int) -> Picture 
mkIndestrutivel pI (x,y) = translate (x1-12.5) (y1+12.5) pI 
                              where (x1,y1) = toReferential (x,y)                  

                              
mkWallIndest :: Estado -> Picture -> Picture
mkWallIndest e pI = pictures(map (mkIndestrutivel pI) lpos)
                        where lpos = obtemMapa (tiraBordas (0,0) (mapaEstado e)) (Bloco Indestrutivel) (1,0)

-- | Obtem Cordenadas Do Mapa relativamente a um tipo de Peca
tiraBordas :: (Int,Int) -> Mapa -> Mapa
tiraBordas _ [] = []
tiraBordas (x,y) m =if acabou then m else (if final then proximalinha else (if borda then atualizaVazia else sematualizar))
                              where final = (y == snd ((dimensaoMatriz m)))
                                    proximalinha = tiraBordas (x+1,0) m
                                    borda = eBordaMatriz (x,y) m
                                    atualizaVazia = tiraBordas (x,y+1) (atualizaPosicaoMatriz (x,y) Vazia m)
                                    sematualizar = tiraBordas (x,y+1) m 
                                    acabou = (x == fst ((dimensaoMatriz m)))
 
obtemMapa :: Mapa -> Peca -> (Int,Int) -> [(Int,Int)]
obtemMapa [] _ _ = []
obtemMapa ([]:t) peca (x,y) = obtemMapa t peca (0,y+1) 
obtemMapa e@((h:hs):t) peca (x,y) = if h == peca then (x,y):obtemMapa (hs:t) peca (x+1,y) else obtemMapa (hs:t) peca (x+1,y)
                                                                        
-- | Atualiza coordenadas para o referencial 0 se for um jogador outra coisa se for objetos do mapa
toReferential ::(Int,Int) -> (Float,Float)
toReferential (x,y) = (realToFrac ((tamanhoBloco*x)+posInicialX),realToFrac(posInicialY - (tamanhoBloco*y)))

-- | Desenha a linha do jogo
mkLinha :: Estado -> Picture
mkLinha estado = line (posicaoBorda mapa)
                  where mapa = dimensaoMatriz (mapaEstado estado)

-- | Retorna as Posicoes das bordas ja no referencial de forma a desenhar a linha
posicaoBorda:: Dimensao -> [(Float,Float)]
posicaoBorda d =  [toReferential (0,0) , toReferential (b,0),toReferential (b,a),toReferential (0,a),toReferential(0,0)]
                  where a = fst d -2
                        b = snd d -2
-- [translate 716.0 0.0 (caixa)] ++ [translate 716.0 -150.0 (caixa)]
-- caixa = line (posicaoBorda (8,14))

-- recebo os jogadores que estão no estado e dou as infos que do jogo
mkinfo :: Estado -> Picture
mkinfo e = pictures [players]
            where players = infoJog (jogadoresEstado e) 

generalizada :: Estado -> Picture
generalizada e = undefined


-- faz os varios quadrados dando um translate diferente consuante o int 
-- Color (greyN 1) $ Polygon (rectanglePath 26 26)
infoJog ::  [Jogador] -> Picture 
infoJog [] = blank
infoJog (a:b:c:d:t) = pictures(box1 ++ box2 ++ box3 ++ box4 ++ [jogador1 a] ++ [jogador2 b] ++ [jogador3 c] ++ [jogador4 d])
                        where caixa = color (greyN 0.9) $ Polygon (posicaoBorda (8,14))
                              caixa2 = line (posicaoBorda (8,14))
                              box1 = [translate 716.0 0.0 (caixa)] ++ [translate 716.0 0.0 (caixa2)]
                              box2 = [translate 716.0 (-150.0) (caixa)] ++ [translate 716.0 (-150.0) (caixa2)]
                              box3 = [translate 716.0 (-300.0) (caixa)] ++ [translate 716.0 (-300.0) (caixa2)]
                              box4 = [translate 716.0 (-450.0) (caixa)] ++ [translate 716.0 (-450.0) (caixa2)]
                              

--o quadrado de um jogador
jogador1 :: Jogador -> Picture
jogador1 j = pictures([info1] ++ [info2] ++ [info3])
            where info1 =  (translate (500.0) (260.0) $ scale 0.1 0.1 $ text "P1     KEYS = ARROWS , . -")
                  info2 =  (translate (500.0) (220.0) $ scale 0.1 0.1 $ text ("CANHOES : Infinitos   LASERS : " ++ (show (lasersJogador j))))
                  info3 =  (translate (500.0) (180.0) $ scale 0.1 0.1 $ text ("CHOQUES : " ++ (show (choquesJogador j)) ++"     VIDAS : " ++ (show (vidasJogador j))))



--o quadrado de um jogador
jogador2 :: Jogador -> Picture
jogador2 j = pictures([info1] ++ [info2] ++ [info3])
            where info1 =  (translate 500 (110.0) $ scale 0.1 0.1 $ text "P2     KEYS = WASD 123")
                  info2 = (translate 500 (70.0) $ scale 0.1 0.1 $ text ("CANHOES : Infinitos  LASERS : " ++ (show (lasersJogador j))))
                  info3 =  (translate 500 (30.0) $ scale 0.1 0.1 $ text ("CHOQUES : " ++ (show (choquesJogador j)) ++  "     VIDAS : " ++ (show (vidasJogador j))))


--o quadrado de um jogador
jogador3 :: Jogador -> Picture
jogador3 j = pictures([info1] ++ [info2] ++ [info3])
            where info1 =  (translate 500 (-40.0) $ scale 0.1 0.1 $ text "P3     KEYS = TFGH 456")
                  info2 =  (translate 500 (-80.0) $ scale 0.1 0.1 $ text ("CANHOES : Infinitos  LASERS : " ++ (show (lasersJogador j))))
                  info3 =  (translate 500 (-120.0) $ scale 0.1 0.1 $ text ("CHOQUES : " ++ (show (choquesJogador j)) ++  "     VIDAS : " ++ (show (vidasJogador j))))


--o quadrado de um jogador
jogador4 :: Jogador -> Picture
jogador4 j = pictures([info1] ++ [info2] ++ [info3])
            where info1 =  (translate 500 (-180.0) $ scale 0.1 0.1 $ text "P4     KEYS = JiKL 789")
                  info2 =  (translate 500 (-220.0) $ scale 0.1 0.1 $ text ("CANHOES : Infinitos  LASERS : " ++ (show (lasersJogador j))))
                  info3 =  (translate 500 (-260.0) $ scale 0.1 0.1 $ text ("CHOQUES : " ++ (show (choquesJogador j)) ++ "     VIDAS : " ++ (show (vidasJogador j))))



mkfundo :: Picture -> Picture
mkfundo f = scale 0.8 0.7 f