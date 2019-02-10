
-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Tarefa5_2018li1g101 where
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Tarefa4_2018li1g101
import Tarefa3_2018li1g101
import Tarefa2_2018li1g101
import Tarefa1_2018li1g101
import Tarefa0_2018li1g101
import LI11819
import Mapa
import Controls
import TickRun
import Janela
import EstadosInicial
import Render



-- | Ficheiros com as texturas
{-bmpFiles :: [FilePath]
bmpFiles = ["images/bloco_ind.bmp","images/tank_red.bmp"]-}

-- | Função principal da Tarefa 5. Executa o Jogo
main :: IO ()
main = do 
    --textures <- mapM loadBMP bmpFiles
    tank <- loadBMP "images/tank.bmp"
    pD <- loadBMP "images/par_dest.bmp"
    pI <- loadBMP "images/bloco_ind.bmp"
    disp<- loadBMP "images/disp.bmp"
    laser <-loadBMP "images/laser.bmp"
    fundo <- loadBMP "images/fundo.bmp"                              -- Load das texturas                  
    play window                                                  -- Janela Principal onde corre o jogo
        background                                               -- Cor de fundo da janela 
        fr                                                       -- Frame rate
        (estadoGlossInicial estadoTesteRender2 pI fundo)                               -- Estado Inicial
        (desenhaEstadoGloss tank disp laser pD pI)                             -- Desenha o Estado do Jogo
        reageEventoGloss                                         -- Reage a um evento (Pressionar Teclas etc)
        reageTempoGloss                                          -- Reage ao passar do tempo
    

