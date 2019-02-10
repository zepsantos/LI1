-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g101 where
import Tarefa0_2018li1g101
import Tarefa2_2018li1g101
import Tarefa1_2018li1g101
import Data.Maybe
import Tarefa3_2018li1g101()
import LI11819

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [estadoTeste3]

-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funçõs 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers . tickMortos

tickMortos :: Estado -> Estado
tickMortos e = e {disparosEstado = eliminaDisparoDeMortos e (disparosEstado e)}

eliminaDisparoDeMortos :: Estado -> [Disparo] -> [Disparo]
eliminaDisparoDeMortos _ [] = []
eliminaDisparoDeMortos e (h:t) = if vidasJogador jog > 0 then h:eliminaDisparoDeMortos e t else eliminaDisparoDeMortos e t
                              where jog = encontraIndiceLista (jogadorDisparo h) (jogadoresEstado e)
-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers estado = if existelaser (disparosEstado estado) then expandeLaser estado (length (disparosEstado estado)) else estado

-- | vertifa se existe nos disparos do estado lasers
existelaser :: [Disparo] -> Bool
existelaser = foldr ((||) . isLaser) False


-- | exapnde todos os laser que estão no estado e retorna o estado atualizado com todos os disparos dos lasers
expandeLaser :: Estado -> Int -> Estado
expandeLaser estado 0 = estado                          -- BUG: ESTA A DAR ERRO AQUI
expandeLaser estado x = if isLaser && temlaser then (if not borda then expandeLaser nuevoestado (x-1) else expandeLaser novoestado (x-1)) else expandeLaser estado (x-1)
                        where isLaser = isJust laser
                              laser = mEncontraIndiceLista (x-1) (disparosEstado estado)
                              temlaser = isMLaser  laser
                              borda = eBordaMatriz (posicaoDisparo (fromJust laser)) (mapaEstado estado)
                              novoestado = estado{disparosEstado = elimina (fromJust laser) (disparosEstado estado)}
                              nuevoestado = explodeLogo (fromJust laser) (x-1) estado 


mEncontraIndiceLista :: Int -> [Disparo] -> Maybe Disparo
mEncontraIndiceLista _ [] = Nothing
mEncontraIndiceLista 0 l =  Just $ head l
mEncontraIndiceLista n (h:t) = mEncontraIndiceLista (n-1) t

    

explodeLogo :: Disparo -> Int -> Estado -> Estado
explodeLogo d x e = if ce then (if replicavel then andaLaser d x tankex estadoexp else saiLaser d (fst ( explosaoLaser e []))) else andaLaser d x [] e 
                        where ce = cimaeEsq d
                              replicavel = podeReplicar d e
                              tankex = snd (explosaoLaser e [])
                              estadoexp = fst (explosaoLaser e [])

cimaeEsq :: Disparo -> Bool
cimaeEsq d = (direcaoDisparo d)== C || (direcaoDisparo d) == E                             
                        
destroiParede2 :: Peca -> Posicao -> Estado -> Estado
destroiParede2 (Bloco Destrutivel) pos estado = estado{mapaEstado = atualizaPosicaoMatriz pos Vazia (mapaEstado estado)}
destroiParede2  _ _ estado = estado

-- | replica um laser pela linha toda até atingir uma parede indestrutivel e atualiza o estado com os jogadores atingidos e as paredes destruidas
andaLaser :: Disparo -> Int -> [Jogador] -> Estado -> Estado
andaLaser laser n a estado = if semIndes then andaLaser newlaser n jogExplodidos estadoExplodido else (if ambosIndes then estadofinal else saiLaser newlaser (fst (explosaoLaser estadofinal2 tanks)))
                         where newlaser = novoDisparo laser
                               semIndes = podeReplicar newlaser estado 
                               ambosIndes = bothIndes newlaser estado
                               newEstado = estado{disparosEstado = atualizaIndiceLista n newlaser (disparosEstado estado)}
                               lasereliminado = elimina laser (disparosEstado estado)
                               lasereliminado2 = elimina newlaser (disparosEstado estado)
                               estadoExplodido = fst (explosaoLaser newEstado a)
                               jogExplodidos = snd (explosaoLaser newEstado a)
                               estadofinal = fst (explosaoLaser estado{disparosEstado = lasereliminado} a)
                               estadofinal2 = adiciona newlaser estadofinal    
                               tanks = snd (explosaoLaser estado{disparosEstado = lasereliminado} a)



saiLaser :: Disparo -> Estado -> Estado
saiLaser d e = e{disparosEstado = elimina d (disparosEstado e)}


bothIndes :: Disparo -> Estado -> Bool
bothIndes d e = if (tiroVertical (direcaoDisparo d)) then pindesY && sindesY else pindesX && sindesX
                                                where pindesY = ((encontraPosicaoMatriz (posicaoDisparo d) (mapaEstado e)) == Bloco Indestrutivel)
                                                      sindesY = ((encontraPosicaoMatriz (incrementaY (posicaoDisparo d)) (mapaEstado e)) == Bloco Indestrutivel)
                                                      pindesX = ((encontraPosicaoMatriz (posicaoDisparo d) (mapaEstado e)) == Bloco Indestrutivel)
                                                      sindesX = ((encontraPosicaoMatriz (incrementaX (posicaoDisparo d)) (mapaEstado e)) == Bloco Indestrutivel)

adiciona :: Disparo -> Estado -> Estado
adiciona d e = e{disparosEstado = [d] ++ (disparosEstado e)}



-- | elemina um disparo da lista de disparos do estado
elimina :: Disparo -> [Disparo] -> [Disparo]
elimina _ [] = []
elimina l (h:t) | jogadorDisparo l == jogadorDisparo h && posicaoDisparo l == posicaoDisparo h && direcaoDisparo l == direcaoDisparo h = t
                | otherwise = h:elimina l t


-- | anda com o laser para a frente
novoDisparo :: Disparo -> Disparo
novoDisparo laser = laser{posicaoDisparo = somaVetores (posicaoDisparo laser) (direcaoParaVetor (direcaoDisparo laser))}


-- | verifica se o laser pode replicar ou seja nao tem parece indestrutivel a sua frente
podeReplicar :: Disparo -> Estado -> Bool
podeReplicar l estado = if vertical then inicial && inicialY else inicial && inicialX 
                              where vertical = tiroVertical (direcaoDisparo l)
                                    inicial = encontraPosicaoMatriz (posicaoDisparo l) (mapaEstado estado) /= Bloco Indestrutivel
                                    inicialY = encontraPosicaoMatriz (incrementaY (posicaoDisparo l)) (mapaEstado estado) /= Bloco Indestrutivel
                                    inicialX = encontraPosicaoMatriz (incrementaX (posicaoDisparo l)) (mapaEstado estado) /= Bloco Indestrutivel

-- | ve se o disparo que esta no estado é um laser
isLaser :: Disparo -> Bool
isLaser DisparoLaser{jogadorDisparo = _ , posicaoDisparo = _ , direcaoDisparo = _} = True
isLaser _ = False

isMLaser :: Maybe Disparo -> Bool
isMLaser (Just DisparoLaser{jogadorDisparo = _ , posicaoDisparo = _ , direcaoDisparo = _}) = True
isMLaser _ = False

-- | explode o laser ou seja danifica o que tem de danificar i.e jogadores paredes e outros disparos
explosaoLaser :: Estado -> [Jogador] -> (Estado,[Jogador])
explosaoLaser estado a = (Estado{mapaEstado = mapaEstado novomapa,jogadoresEstado = jogadoresEstado novosJog,disparosEstado = novosDisp},jogAtingidos)
                        where novomapa = newMapa (disparosEstado estado) estado
                              novosJog = fst (newJogadores (disparosEstado estado) a estado)
                              jogAtingidos = snd (newJogadores (disparosEstado estado) a estado)
                              novosDisp = newDisparos (disparosEstado estado) (disparosEstado estado)


-- | new mapa recebe os disparos que estao no estado e o estado e retorna o estado com o mapa atualizado i.e paredes destrutiveis atingidas detruidas
newMapa :: [Disparo] -> Estado -> Estado
newMapa [] estado = estado
newMapa (h:t) estado = if isLaser h then estado{mapaEstado = mapaEstado newEstado2} else newMapa t estado
                  where newEstado =  destroiParede2 (encontraPosicaoMatriz (posicaoDisparo h) (mapaEstado estado)) (posicaoDisparo h) estado
                        newEstado2 = if tiroVertical (direcaoDisparo h) then destroiParede2 (encontraPosicaoMatriz (incrementaY (posicaoDisparo h)) (mapaEstado newEstado)) (incrementaY (posicaoDisparo h)) newEstado else destroiParede2 (encontraPosicaoMatriz (incrementaX (posicaoDisparo h)) (mapaEstado newEstado)) (incrementaX (posicaoDisparo h)) newEstado


-- | incrementa o y a uma posicao
incrementaY :: Posicao -> Posicao
incrementaY (x,y) = (x,y+1)

-- | incrementa o x a uma posicao
incrementaX :: PosicaoGrelha  -> PosicaoGrelha 
incrementaX (x,y) = (x+1,y)

-- | incrementa ambos o x e o y a uma posicao
incrementaAmbos :: PosicaoGrelha  -> PosicaoGrelha 
incrementaAmbos (x,y) = (x+1,y+1)

-- | verifica se um tiro é vertical
tiroVertical :: Direcao -> Bool
tiroVertical a = a == C || a == B

-- | recebe os disparos dos estado, os jogares ja atingidos, o estado e retorna o estado com os jogadores atualizados e a lista de jogadores atingidos atualizada
newJogadores :: [Disparo] -> [Jogador] -> Estado -> (Estado,[Jogador])
newJogadores [] x estado = (estado,x)
newJogadores (h:t) a estado = if isLaser h then newJogadores t novosJog2 newestado2 else newJogadores t a estado
                        where newestado = estado{jogadoresEstado = fst (atingiuAlgTank (posicaoDisparo h) a (jogadoresEstado estado))}
                              newestado2 = if etirovertical then newEstadoY else newEstadoX
                              novosJog = snd (atingiuAlgTank (posicaoDisparo h) a (jogadoresEstado estado))
                              novosJog2 = if etirovertical then snd (atingiuAlgTank (incrementaY (posicaoDisparo h)) (novosJog) (jogadoresEstado newestado)) else snd (atingiuAlgTank (incrementaX (posicaoDisparo h)) (novosJog) (jogadoresEstado newestado))
                              etirovertical = tiroVertical (direcaoDisparo h)
                              newEstadoY = estado{jogadoresEstado = fst (atingiuAlgTank (incrementaY (posicaoDisparo h)) novosJog (jogadoresEstado newestado))}
                              newEstadoX = estado{jogadoresEstado = fst (atingiuAlgTank (incrementaX (posicaoDisparo h)) novosJog (jogadoresEstado newestado))}


-- | recebe a posicao do disparo, a lista de jogadores atingidos e a lista de jogadores do estado e retorna a lista de jogadores do estado atualizada e a lista de jogadores atingidos atualizada
atingiuAlgTank :: Posicao -> [Jogador] -> [Jogador] -> ([Jogador],[Jogador])
atingiuAlgTank _ x [] = ([],x)
atingiuAlgTank pos a (h:t) = if jogadorAtingido a h then estadoatingido else (if jogadorbaleado then estadoatualizado else estadoatingido )
                  where estadoatingido = insereEle h (atingiuAlgTank pos a t)
                        estadoatualizado = insereList (atualizaVida [h]) (atingiuAlgTank pos (insereJog h a) t)
                        jogadorbaleado =  pos == posicaoJogador h || pos == incrementaY (posicaoJogador h) || pos == incrementaX (posicaoJogador h) || pos == incrementaAmbos (posicaoJogador h)

-- | verifica se um jogador ja foi atingido
jogadorAtingido :: [Jogador]  -> Jogador -> Bool
jogadorAtingido [] _ = False
jogadorAtingido (h:t) x = jogadoresIguais h x || jogadorAtingido t x

-- | verfica se dois jogadores são o mesmo
jogadoresIguais :: Jogador -> Jogador -> Bool
jogadoresIguais a b = posicaoJogador a == posicaoJogador b && direcaoJogador a == direcaoJogador b && lasersJogador a == lasersJogador b && choquesJogador a == choquesJogador b

-- | insere um jogador numa lista de jogadores
insereJog :: Jogador -> [Jogador] -> [Jogador]
insereJog x a = x:a

-- | insere um jogador na 1º lista de jogadores do par 
insereEle :: Jogador -> ([Jogador],[Jogador]) -> ([Jogador],[Jogador])
insereEle x (a,b) = (x:a,b)

-- | insere uma lista de jogadores na 1º lista do par
insereList :: [Jogador] -> ([Jogador],[Jogador]) -> ([Jogador],[Jogador])
insereList x (a,b) = (x++a,b)

-- | atualiza a vida de um jogador ou seja decrementa-a quando é atingido pelo laser
atualizaVida :: [Jogador] -> [Jogador]
atualizaVida [] = []
atualizaVida (jogador:t) = if vidasJogador jogador == 0 then jogador:t else jogador{vidasJogador = vidasJogador jogador - 1}:t


-- | atualiza a lista de disparos do estado eleminado os disparos canhoes que estão sobrepostos por lasers
newDisparos :: [Disparo] -> [Disparo] -> [Disparo]
newDisparos [] x = x
newDisparos _ [] = []
newDisparos (h:t) (a:b) = if temlaser then (if temCanhao && saoIguais then newDisparos (h:t) b else a:newDisparos (h:t) b)  else newDisparos t (a:b)
                        where temlaser = isLaser h
                              temCanhao = eDisparoCanhao (Just a)
                              saoIguais = comparatiros h a

-- | compara a posicao de dois tiros
comparatiros :: Disparo -> Disparo -> Bool
comparatiros h a = posicaoDisparo h == posicaoDisparo a

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado
tickCanhoes  = trabalhaCanhoes

-- | Ve se os canhoes estao em "conflito"
trabalhaCanhoes :: Estado  -> Estado
trabalhaCanhoes e = newEstado
                  where listOfDisp = map Just (disparosEstado e)
                        newEstado = trabalhaDisparosCanhoes listaSemCanhoesIntercetados (length listaSemCanhoesIntercetados) e
                        listaSemCanhoesIntercetados = canhoesABaterEmCanhoes listOfDisp (length listOfDisp)



--  ESTAS DUAS FUNCOES TAO MAL SO FALTA ISTO
canhoesABaterEmCanhoes ::  [Maybe Disparo] -> Int -> [Maybe Disparo]
canhoesABaterEmCanhoes [] _ = []
canhoesABaterEmCanhoes l 0 = l
canhoesABaterEmCanhoes l n = if eDisparoCanhao disp && acertou  then canhoesABaterEmCanhoes eliminaCanhoes (n-1) else canhoesABaterEmCanhoes l (n-1)
                            where disp = maybeEncontraIndiceListaDisparo (n-1) l 
                                  (acertou,elemDisp) = acertouNumCanhao disp l
                                  eliminaCanhoes = eliminaDisparo elemDisp $ atualizaIndiceLista (n-1) Nothing l

eliminaDisparo :: Maybe Disparo -> [Maybe Disparo] -> [Maybe Disparo]
eliminaDisparo _ [] = []
eliminaDisparo d (h:t) = if isJust d && isJust h && fromJust d == fromJust h then t else h:eliminaDisparo d t   

acertouNumCanhao :: Maybe Disparo -> [Maybe Disparo] -> (Bool,Maybe Disparo)
acertouNumCanhao d (h:t) = if d /= h && eDisparoCanhao d && eDisparoCanhao h && verificaSeBatem  then (True,h) else  acertouNumCanhao d t
                        where verificaSeBatem = ( isJust d && isJust h) && ( posd == posh || posh == subtraiVetores posd (direcaoParaVetor (direcaoDisparo (fromJust d))))
                              posd = posicaoDisparo (fromJust d)
                              posh = posicaoDisparo (fromJust h) 
acertouNumCanhao _ _ = (False,Nothing)

trabalhaDisparosCanhoes :: [Maybe Disparo] -> Int -> Estado -> Estado
trabalhaDisparosCanhoes [] _ e = e
trabalhaDisparosCanhoes l 0 e = e {disparosEstado = mapMaybe nextTickCanhao l}
trabalhaDisparosCanhoes lD n e = if eDisparoCanhao disp then atinge else trabalhaDisparosCanhoes lD (n-1) e
                              where disp = maybeEncontraIndiceListaDisparo (n-1) lD
                                    (eTemp,dispState) =  checkAtingiu e (fromJust disp)
                                    atinge = if isJust dispState then trabalhaDisparosCanhoes lD (n-1) eTemp else trabalhaDisparosCanhoes (atualizaIndiceLista (n-1) Nothing lD) (n-1) eTemp 

nextTickCanhao :: Maybe Disparo -> Maybe Disparo
nextTickCanhao  (Just d@DisparoCanhao{jogadorDisparo = _ , posicaoDisparo=pos , direcaoDisparo=dir}) = Just d{ posicaoDisparo =  somaVetores pos (direcaoParaVetor dir)}
nextTickCanhao d =  d

checkAtingiu :: Estado -> Disparo -> (Estado,Maybe Disparo)
checkAtingiu e d = if e /= newEstado then (newEstado,Nothing) else (e,disp)
                  where (newEstado, disp ) = checkAtingiuParede d $ checkAtingiuJogador e d

checkAtingiuJogador :: Estado -> Disparo -> Estado
checkAtingiuJogador e d = e{jogadoresEstado= acertouNumJogador (jogadoresEstado e) d}

acertouNumJogador :: [Jogador] -> Disparo -> [Jogador]
acertouNumJogador [] _  = []
acertouNumJogador (h:t) d = if isJogadorHit posJog posDisp then h{vidasJogador = decreaseVidasJogador (vidasJogador h)}:acertouNumJogador t d else h:acertouNumJogador t d
                        where posJog = posTank h
                              posDisp = getPosDisp (direcaoDisparo d) (posicaoDisparo d)

-- | Funcao que tira vidas ao Jogador
decreaseVidasJogador :: Int -> Int
decreaseVidasJogador 0 = 0
decreaseVidasJogador n = n-1

-- | Retorna ambas as Posicoes do Disparo
getPosDisp :: Direcao -> Posicao -> (Posicao,Posicao)
getPosDisp C (x,y) = ((x,y),(x,y+1))
getPosDisp B (x,y) = ((x+1,y),(x+1,y+1))
getPosDisp D (x,y) = ((x,y+1),(x+1,y+1))
getPosDisp E (x,y) = ((x,y),(x+1,y))

-- | Verifica se o Jogador (4x4) é atingido pelo Disparo
isJogadorHit :: [Posicao] -> (Posicao,Posicao) -> Bool
isJogadorHit [] _ = False
isJogadorHit (h:t) (p1,p2) = h == p1 || h == p2 || isJogadorHit t (p1,p2)  

-- | Funcao que retorna uma lista de posiçoes dos tanques dos jogadores (4x4)
posTank :: Jogador -> [Posicao]
posTank jog = [(x,y),(x+1,y),(x+1,y+1),(x,y+1)]
            where (x,y) = posicaoJogador jog

checkAtingiuParede :: Disparo -> Estado -> (Estado, Maybe Disparo)
checkAtingiuParede d e = if encBloco then (eTemp,Nothing) else (e,Just d)
                  where posDisp = getPosDisp (direcaoDisparo d) (posicaoDisparo d)
                        (eTemp,encBloco) = atingeParede e posDisp
                  

atingeParede :: Estado -> (Posicao,Posicao) -> (Estado,Bool) 
atingeParede e (pos,pos1) = (newEstado, (incBloco || incBloco2))
                        where peca p = encontraPosicaoMatriz p mapa
                              mapa = mapaEstado e
                              (eTemp,incBloco) = verificaParede (peca pos) pos e
                              (newEstado, incBloco2) = verificaParede (peca pos1) pos1 eTemp                             
-- | Funcao que verifica o tipo de parede e a destroi
verificaParede :: Peca -> Posicao -> Estado -> (Estado,Bool)
verificaParede (Bloco Destrutivel) pos estado = (estado{mapaEstado = atualizaPosicaoMatriz pos Vazia (mapaEstado estado)},True)
verificaParede Vazia _ e = (e,False)
verificaParede  _ _ estado = (estado,True)

maybeEncontraIndiceListaDisparo :: Int -> [Maybe Disparo] -> Maybe Disparo
maybeEncontraIndiceListaDisparo _ [] = Nothing
maybeEncontraIndiceListaDisparo 0 l =  head l
maybeEncontraIndiceListaDisparo n (h:t) = maybeEncontraIndiceListaDisparo (n-1) t

-- | Verifica se o disparo é do tipo Canhao
eDisparoCanhao :: Maybe Disparo -> Bool
eDisparoCanhao (Just DisparoCanhao{jogadorDisparo = _ , posicaoDisparo = _ , direcaoDisparo = _}) = True
eDisparoCanhao _ = False

-- | Funcao que destroi uma parede Destrutivel
destroiParede :: Peca -> Posicao -> Estado -> Estado
destroiParede (Bloco Destrutivel) pos estado = estado{mapaEstado = atualizaPosicaoMatriz pos Vazia (mapaEstado estado)}
destroiParede  _ _ estado = estado

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques  estado = estado {disparosEstado = trabalhaChoques (disparosEstado estado)}

-- | Funcao que decrementa o tick do chock e o elimina caso o tick ja tenha passado.
trabalhaChoques :: [Disparo] -> [Disparo]
trabalhaChoques [] = []
trabalhaChoques  (h@DisparoChoque{jogadorDisparo= _, tempoDisparo = tD}:t) = if tD > 1 then h{tempoDisparo = tempoDisparo h -1}:trabalhaChoques t else trabalhaChoques t
trabalhaChoques (h:t) = h:trabalhaChoques t