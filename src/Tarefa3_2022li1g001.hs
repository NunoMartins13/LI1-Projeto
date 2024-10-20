{- |
Module      : Tarefa3_2022li1g001
Description : Movimentação do personagem e obstáculos
Copyright   : Carlos Eduardo Martins de Sá Fernandes <a100890@alunos.uminho.pt>
              Nuno de Freitas Machado Martins <a96227@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g001 where

import LI12223

import Tarefa1_2022li1g001
import Tarefa4_2022li1g001

-- | Funcao que movimenta os obstáculos (de acordo com a velocidade do terreno em que se encontram) e o personagem, de acordo com a jogada dada
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) mn@(Mapa l m)) Parado = (Jogo (eTronco (Jogador (x,y)) mn 0) (Mapa l (moveMapa m)))
                                                      
animaJogo jj@(Jogo (Jogador (x,y)) ms@(Mapa l m)) (Move d) = if (not (eArvore jj (Move d))) then 
                                                             case d of Cima -> if (y>0) then (Jogo (eTronco (Jogador (x,y-1)) ms 0) (Mapa l (moveMapa m)))
                                                                                        else j
                                                                   
                                                                       Baixo -> if (y+1<length m) then (Jogo (eTronco (Jogador (x,y+1)) ms 0) (Mapa l (moveMapa m)))
                                                                                                  else j 

                                                                       Direita -> if (x<l-1) then (Jogo (eTronco (Jogador (x+1,y)) ms 0) (Mapa l (moveMapa m)))
                                                                                             else j

                                                                       Esquerda -> if (x>0) then (Jogo (eTronco (Jogador (x-1,y)) ms 0) (Mapa l (moveMapa m)))
                                                                                            else j

                                                            else j
                                                               
                                                    where j = (Jogo (eTronco (Jogador (x,y)) ms 0) (Mapa l (moveMapa m)))


                                                            


-- | Move o Jogador
moveJogador :: Jogo -> Jogada -> Jogo  
moveJogador jj@(Jogo (Jogador (x,y)) ms@(Mapa l m)) (Move d) = if (not (eArvore jj (Move d))) then 
                                                             case d of Cima -> if (y>0) then (Jogo (eTronco (Jogador (x,y-1)) ms 0) (Mapa l m))
                                                                                        else j
                                                                   
                                                                       Baixo -> if (y+1<length m) then (Jogo (eTronco (Jogador (x,y+1)) ms 0) (Mapa l m))
                                                                                                  else j 

                                                                       Direita -> if (x<l-1) then (Jogo (eTronco (Jogador (x+1,y)) ms 0) (Mapa l m))
                                                                                             else j

                                                                       Esquerda -> if (x>0) then (Jogo (eTronco (Jogador (x-1,y)) ms 0) (Mapa l m))
                                                                                            else j

                                                            else j
                                                               
                                                    where j = (Jogo (eTronco (Jogador (x,y)) ms 0) (Mapa l m))

-- | Move todas as listas de obstaculos dos terrenos Rio e Estrada
moveMapa :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
moveMapa [] = []
moveMapa ((Rio v,obs):ls) = if v>0 then (Rio v, moveAuxDir v obs):(moveMapa ls)
                                   else (Rio v, moveAuxEsq v obs):(moveMapa ls)

moveMapa ((Estrada v,obs):ls) = if v>0 then (Estrada v, moveAuxDir v obs):(moveMapa ls)
                                       else (Estrada v, moveAuxEsq v obs):(moveMapa ls)

moveMapa ((Relva,obs):ls) = (Relva, obs):(moveMapa ls)

        
-- | Move a lista n elementos para a Direita
moveAuxDir :: Int -> [Obstaculo] -> [Obstaculo]
moveAuxDir n l |n==1 = [last l]++(init l)
               |otherwise = moveAuxDir (n-1) ([last l]++(init l))

-- | Move a lista n elementos para a Esquerda
moveAuxEsq :: Int -> [Obstaculo] -> [Obstaculo]
moveAuxEsq n l |n==(-1) = (tail l)++[head l]
               |otherwise = moveAuxEsq (n+1) ((tail l)++[head l])


-- | Funcao que move jogador quando este se encontra em cima de um tronco, onde n representa o indice da linha do mapa
eTronco :: Jogador -> Mapa -> Int -> Jogador
eTronco (Jogador (x,y)) (Mapa l []) n = Jogador (x,y)
eTronco (Jogador (x,y)) (Mapa l ((Rio v,obs):ls)) n = if ((obs!!x) == Tronco) && (y==n) && (x+v>=0 && x+v<=l-1) then (eTronco (Jogador (x+v,y)) (Mapa l ls) (n+1))
                                                                                                                else (eTronco (Jogador (x,y)) (Mapa l ls) (n+1))
eTronco (Jogador (x,y)) (Mapa l ((_,_):ls)) n = eTronco (Jogador (x,y)) (Mapa l ls) (n+1)


-- | Funcao que verifica se é posssivel um certa jogada consoante as arvore ao redor do jogador, devolvendo True se houver uma arvore a obstruir a jogada
eArvore :: Jogo -> Jogada -> Bool
eArvore (Jogo (Jogador (x,y)) (Mapa l m)) (Move d) = case d of Cima -> if (y>0) then ((listaObstaculos m)!!(y-1)!!x == Arvore)
                                                                                      else False
                                                                     
                                                               Baixo -> if (y+1<length m) then ((listaObstaculos m)!!(y+1)!!x == Arvore)
                                                                                                else False

                                                               Direita -> if (x<l-1) then ((listaObstaculos m)!!y!!(x+1) == Arvore)
                                                                                           else False

                                                               Esquerda -> if (x>0) then ((listaObstaculos m)!!y!!(x-1) == Arvore)
                                                                                          else False

                                                                                      

-- | Funcao que interrompe o movimento do obstaculo Carro quando o player é atropelado                                                                                                                                
--atropelamento :: Jogo -> Jogada -> Jogo
--atropelamento j@(Jogo (Jogador (x,y)) (Mapa l [(Estrada v,obs)])) Parado | debaixoCarro (animaJogo j Parado) = if ((moveAuxDir (v-1) obs)!!x == Carro) then (Jogo (Jogador (x,y)) (Mapa l [(Estrada v,moveAuxDir (v-1) obs)]))
--                                                                                                                                                      else (animaJogo j Parado)
--                                                                        | otherwise = (animaJogo j Parado)



