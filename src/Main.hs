module Main where

import LI12223
import Tarefa1_2022li1g001
import Tarefa2_2022li1g001
import Tarefa3_2022li1g001
import Tarefa4_2022li1g001
import Tarefa6_2022li1g001
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture 
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import System.Directory

--frame rate do jogo
fr :: Int  
fr = 1

--janela do jogo
dm :: Display 
dm = InWindow "Crossy Road" (384,576) (0,0)

main :: IO ()
main = do 
	player  <- loadBMP "img/player.bmp" 
	relva   <- loadBMP "img/relva.bmp"
	arvore  <- loadBMP "img/arvore.bmp"
	estrada <- loadBMP "img/estrada.bmp"
	carro   <- loadBMP "img/carro.bmp"
	rio     <- loadBMP "img/rio.bmp"
	tronco  <- loadBMP "img/tronco.bmp"
	gameOver<- loadBMP "img/gameOver.bmp"
	state   <- loadGame "save.txt"
	     [
         ("-Carro", carro),
         ("+Carro", Rotate 180 carro),
         ("Tronco", tronco),
         ("Arvore", arvore),
         ("Relva", relva),
         ("Estrada", estrada),
         ("Rio", Rotate 90 rio),
         ("Player", Scale 0.75 0.75 player),
         ("GameOver", Scale 0.3 0.3 gameOver)
	     ]  	
        
         (Mapa 3
	  	 [(Estrada (-1), [Nenhum,Carro,Nenhum]),
	  	  (Relva, [Nenhum,Arvore,Nenhum]),
          (Rio 1, [Nenhum,Tronco,Tronco]),
          (Relva, [Nenhum,Nenhum,Arvore])
          
	  	 ]
	     )
	playIO
	  dm
	  white
	  fr
	  state
	  desenhaEstadoGloss
	  reageEventoGloss
	  reageTempoGloss
