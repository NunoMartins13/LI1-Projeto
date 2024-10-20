{- |
Module      : Tarefa6_2022li1g001
Description : Aplicação gráfica completa
Copyright   : Carlos Eduardo Martins de Sá Fernandes <a100890@alunos.uminho.pt>
              Nuno de Freitas Machado Martins <a96227@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}
module Tarefa6_2022li1g001 where

import LI12223
import Tarefa3_2022li1g001
import Tarefa4_2022li1g001
import Tarefa5_2022li1g001
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import System.Exit
import System.Directory


-- | Estado do player no gloss (coordenadas gloss)
type Estado = (Float, Float)

-- | Estado do gloss composto pelas coordenadas do player, o mapa, as textures e o score)
type EstadoGloss = (Estado, Mapa, Textures, Int)

-- | As textures fazem correspondência entre as strings e a respetiva picture por exemplo: ("Relva", relva)
type Textures = [(String, Picture)]

-- | Coordenadas iniciais do player
estadoInicial :: Estado 
estadoInicial = (1,3)

-- | Estado inicial do gloss
estadoGlossInicial :: Textures -> Mapa -> Int -> EstadoGloss
estadoGlossInicial z m 0 = (estadoInicial, m, z, 0)

-- | Obtém o estado a partir do jogo
getEstado :: Jogo -> Estado 
getEstado (Jogo (Jogador (x,y)) _) = (fromIntegral x, fromIntegral y)

-- | Obtém o mapa a partir do jogo 
getMap :: Jogo -> Mapa  
getMap (Jogo _ m) = m

-- | Obtém um estado gloss a partir de um Jogo e das textures 
getEstadoGloss :: Jogo -> Textures -> Int -> EstadoGloss  
getEstadoGloss jj@(Jogo j m) z s = (getEstado jj, m, z, s)

-- | Obtém um Jogo a partir do estado do gloss (processo inverso da getEstadoGloss)
estadoParaJogo :: EstadoGloss -> Jogo 
estadoParaJogo ((x,y), m, z, s) = (Jogo (Jogador (round x, round y)) m)

-- | Função que recebe uma instrução de movimento e traduz-la num estado gloss, recorrendo à função moveJogador
reageEventoGloss :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) ((x,y), m, z, s) = return $ getEstadoGloss (moveJogador (Jogo (Jogador (round x, round y)) m) (Move Baixo)) z s
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) ((x,y), m, z, s) = return $ getEstadoGloss (moveJogador (Jogo (Jogador (round x, round y)) m) (Move Cima)) z s
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) ((x,y), m, z, s) = return $ getEstadoGloss (moveJogador (Jogo (Jogador (round x, round y)) m) (Move Esquerda)) z s
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) ((x,y), m, z, s) = return $ getEstadoGloss (moveJogador (Jogo (Jogador (round x, round y)) m) (Move Direita)) z s
reageEventoGloss (EventKey (Char 'q') Down _ _) e = do 
                                                    saveGame e
                                                    putStrLn "FIM"
                                                    exitSuccess

reageEventoGloss _ e = return e

-- | Função que altera o mapa gloss em função do tempo, recorrendo à função animaJogo
reageTempoGloss :: Float -> EstadoGloss -> IO EstadoGloss
reageTempoGloss n e@((x,y), m, z, s) = if jogoTerminou (estadoParaJogo e) then return $ getEstadoGloss (animaJogo (Jogo (Jogador (1, 3)) m) Parado) z 0
                                                                          else return $ getEstadoGloss (animaJogo (Jogo (Jogador (round x, round y)) m) Parado) z (s+1)
                                                                      --Teste deslizaJogo
                                                                          --else if (y<3) then return $ getEstadoGloss (deslizaJogo 1 (animaJogo (Jogo (Jogador (round x, round y)) m) Parado)) z (s+1)
                                                                                        --else return $ getEstadoGloss (animaJogo (Jogo (Jogador (round x, round y)) m) Parado) z (s+1)

largura :: Float
largura = 224

comprimento :: Float
comprimento = -128

-- | Converte um terreno com a componente velocidade numa string
terrToStr :: Terreno -> String
terrToStr (Rio _) = "rio"
terrToStr (Estrada v) = if v>0 then "+est"
                               else "-est"
-- | Função auxiliar da mapaParaMatriz
aux :: (Terreno, [Obstaculo]) -> [String]
aux (t, []) = []
aux (t, (o:obs))
 |t == Relva = if o == Nenhum then "Relva":aux (t, obs)
                              else "Arvore":aux (t,obs)
 
 |terrToStr t == "rio" = if o == Nenhum then "Rio":aux (t,obs)
                                        else "Tronco":aux (t,obs)
 
 |terrToStr t == "-est" = if o == Nenhum then "Estrada":aux (t,obs)
                                         else "-Carro":aux (t,obs)

 |terrToStr t == "+est" = if o == Nenhum then "Estrada":aux (t,obs)
                                         else "+Carro":aux (t,obs)

-- | Transforma um Mapa numa matriz de strings
mapaParaMatriz :: Mapa -> [[String]]
mapaParaMatriz (Mapa l []) = []
mapaParaMatriz (Mapa l (h:t)) = aux h : mapaParaMatriz (Mapa l t)

-- | Desenha o jogo na totalidade
desenhaEstadoGloss :: EstadoGloss -> IO Picture
desenhaEstadoGloss s@((x,y), mapa, textures, score) = return $ Pictures desenho
   where desenho = desenhoMapa
                 ++ [desenhoJogador]
                 ++ [desenhoScore]
                 ++ [scoretexto]
         desenhoMapa = desenhaMapa comprimento largura (mapaParaMatriz mapa) textures
         desenhoJogador = if jogoTerminou (estadoParaJogo s) then desenhaGameOver textures
                                                             else desenhaJogador ((x*128)-128,(y*(-128))+224) textures
         desenhoScore = desenhaScore score
         scoretexto = Translate (-180) (-270) $ scale 0.25 0.25 $ text ("Score:")
 
-- | Desenha um obstáculo
desenhaObstaculo :: Float -> Float -> String -> Textures -> Picture
desenhaObstaculo x y obs (h:t)  
    | obs == fst h = Translate x y (snd h)   
    | otherwise = desenhaObstaculo x y obs t    

-- | Desenha uma linha do mapa utilizando a desenhaObstaculo recursivamente
desenhaLinha :: Float -> Float -> [String] -> Textures -> [Picture]
desenhaLinha x y [] textures = []
desenhaLinha x y (h:t) textures = obstaculo : resto
  where obstaculo = desenhaObstaculo x y h textures
        resto = desenhaLinha (x+128) y t textures

-- | Desenha o mapa recorrendo à desenhaLinha recursivamente
desenhaMapa :: Float -> Float -> [[String]] -> Textures -> [Picture]
desenhaMapa x y (h:t) textures = linha ++ resto
  where linha = desenhaLinha x y h textures
        resto = desenhaMapa x (y-128) t textures
desenhaMapa _ _ _ _            = []

-- | Desenha o jogador
desenhaJogador :: Estado -> Textures -> Picture
desenhaJogador (x,y) (h:t)
  | "Player" == fst h = Translate x y (snd h)   
  | otherwise = desenhaJogador (x,y) t

-- | Desenha a mensagem de Game Over
desenhaGameOver :: Textures -> Picture  
desenhaGameOver z = snd (last z)

-- | Desenha o score
desenhaScore :: Int -> Picture 
desenhaScore score = Translate (-80) (-270) $ scale 0.25 0.25 $ text (show score)


{- Save Game
-}

-- | Transforma o estado gloss em string para posteriormente guardar num ficheiro txt
codificaEstadoGloss :: EstadoGloss -> String
codificaEstadoGloss (estado, mapa, texturas, score) =
  show (estado) ++ "\n" ++
  show (mapa) ++ "\n" ++
  show (score) ++ "\n"

-- | Transforma a string num estado gloss
descodificaString :: String -> Textures -> EstadoGloss 
descodificaString str textures = 
  let (linhaestado:linhamapa:linhascore:_) = lines str 
      estado = read linhaestado :: (Float,Float)
      mapa = read linhamapa :: Mapa 
      score = read linhascore :: Int  
  in (estado,mapa,textures,score)

-- | Escreve o ficheiro txt recorrendo ao codificaEstadoGloss
saveGame :: EstadoGloss -> IO ()
saveGame state = writeFile "save.txt" (codificaEstadoGloss state)

-- | Faz a leitura do ficheiro txt, caso este exista devolve o estado gloss lá escrito. Caso contrário devolve o estado inicial do jogo
loadGame :: FilePath -> Textures -> Mapa -> IO EstadoGloss 
loadGame filepath textures mapa = do
  fileExist <- doesFileExist filepath
  if fileExist then do str <- readFile filepath
                       return (descodificaString str textures)
               else return (estadoGlossInicial textures mapa 0)