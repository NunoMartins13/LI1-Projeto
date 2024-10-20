{- |
Module      : Tarefa5_2022li1g001
Description : Deslize do mapa
Copyright   : Carlos Eduardo Martins de Sá Fernandes <a100890@alunos.uminho.pt>
              Nuno de Freitas Machado Martins <a96227@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g001 where

import LI12223
import Tarefa2_2022li1g001

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo n (Jogo (Jogador (x,y)) (Mapa l m)) = Jogo (Jogador (x,y+1)) (estendeMapa (Mapa l (init m)) n)

