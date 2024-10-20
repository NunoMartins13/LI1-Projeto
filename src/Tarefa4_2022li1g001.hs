{- |
Module      : Tarefa4_2022li1g001
Description : Determinar se o jogo terminou
Copyright   : Carlos Eduardo Martins de Sá Fernandes <a100890@alunos.uminho.pt>
              Nuno de Freitas Machado Martins <a96227@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g001 where

import LI12223

import Tarefa1_2022li1g001


-- | Funcao que verifica se o jogo terminou, ou seja, se o jogador esta na agua, fora do mapa ou debaixo de um carro, devolvendo True caso se confirme
jogoTerminou :: Jogo -> Bool
jogoTerminou j = if not (foraMapa j) then (naAgua j) || (debaixoCarro j) 
                                     else True


-- | Funcao que verifica se o jogador esta "debaixo" de um carro devolvendo True se sim
debaixoCarro :: Jogo -> Bool 
debaixoCarro (Jogo (Jogador (x,y)) (Mapa l m)) = (listaObstaculos m)!!y!!x == Carro  


-- | Funcao que verifica se o jogador esta fora do mapa devolvendo True se sim
foraMapa :: Jogo -> Bool
foraMapa (Jogo (Jogador (x,y)) (Mapa l m)) = x>=l || x<0 || y<0 || y>=length m


-- | Funcao que verifica se o jogador esta na agua devolvendo True se sim
naAgua :: Jogo -> Bool
naAgua (Jogo (Jogador (x,y)) (Mapa l m)) = ((listaObstaculos m)!!y!!x == Nenhum) && ((remVel(listaTerrenos m))!!y == "rio")