module Menu where 
import Graphics.Gloss.Interface.Pure.Game

-- | Função que renderiza o menu de Pausa
menuPausa :: Picture
menuPausa = Translate (-200) 0 $ scale 0.4 0.4 $Text "Jogo em Pausa"

-- | Função que renderiza o menu de Inicio
menuInicio :: Picture
menuInicio = Translate (-300) 0 $ scale 0.4 0.4 $Text "Pressione P para Jogar"