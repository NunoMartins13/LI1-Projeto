{- |
Module      : Tarefa2_2022li1g001
Description : Geração contínua de um mapa
Copyright   : Carlos Eduardo Martins de Sá Fernandes <a100890@alunos.uminho.pt>
              Nuno de Freitas Machado Martins <a96227@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g001 where

import LI12223
import Data.Char
import Data.List
import System.Random

-- | Função que estende o Mapa
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa m@(Mapa l lst@((t,obs):to)) seed = Mapa l ((et,go):lst)
                        
      where et = escolherTerreno (proximosTerrenosValidos m) seed l ti
            go = gerarObstaculos l (et,[]) seed
            ti = terrenosIguais lst


-- | Funcao que dá a lista de terrenos válidos para a proxima linha do dado mapa
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa l lst@((t,obs):to)) | t == Rio 0 && ti > 3 = [Estrada 0, Relva]
                                                  | t == Estrada 0 && ti > 4 = [Rio 0, Relva]
                                                  | t == Relva && ti > 4 = [Rio 0, Estrada 0]
                                                  | otherwise = [Rio 0, Estrada 0, Relva]
      where ti = terrenosIguais lst

-- | Funcao que dá a lista de obstaculos válidos a serem adicionados na linha
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos l (t, []) = case t of (Rio _) -> [Nenhum, Tronco]
                                                (Estrada _) -> [Nenhum, Carro]
                                                (Relva) -> [Nenhum, Arvore]                                           
proximosObstaculosValidos l (t,obs) | l <= lo = []
                                    | lv && t == Rio 0 && oi < 5 && ((l-lo>1) || (Nenhum `elem` obs)) = [Nenhum, Tronco]
                                    | lv && t == Estrada 0 && oi < 3 && ((l-lo>1) || (Nenhum `elem` obs)) = [Nenhum, Carro]
                                    | t == Relva && ((l-lo>1) || (Nenhum `elem` obs)) = [Nenhum, Arvore]
                                    | otherwise = [Nenhum]
      where lo = length obs
            oi = obstaculosIguais obs 0
            lv = l > lo

 {- |Funcoes auxiliares 
-}

-- | Funcao que devolve o numero de terrenos iguais contíguos
terrenosIguais :: [(Terreno, [Obstaculo])] -> Int
terrenosIguais ((t,obs):[]) = 1
terrenosIguais (p1@(t,obs):p2@(ts,obs2):to) | (strTerreno p1) == (strTerreno p2) = 1 + terrenosIguais (p2:to)
                                            | otherwise = 1

-- | Funcao que recebe um terreno e devolve a string correspondente, util para remover velocidades
strTerreno :: (Terreno, [Obstaculo]) -> String
strTerreno (t,obs) = case t of (Rio _) -> "rio"
                               (Estrada _) -> "est"
                               (Relva) -> "rel"

-- | Funcao que conta o numero de obstaculos iguais no final da lista de obstaculos
obstaculosIguais :: [Obstaculo] -> Int -> Int 
obstaculosIguais (o:[]) n | o /= Nenhum = n+1
                          | otherwise = 0
obstaculosIguais (o:os:oss) n | o /= Nenhum && o == os = obstaculosIguais (os:oss) (n+1)
                              | otherwise = obstaculosIguais (os:oss) (n-n)

-- | Funcao que  gera lista de inteiros psuedo-aleatorios de len elementos
listaAleatorios :: Int -> Int -> [Int]
listaAleatorios rand len = take len (randoms (mkStdGen rand))

-- | Funcao que escolhe pseudo-aleatoriamente um dos terrenos validos para a proxima linha do mapa
escolherTerreno :: [Terreno] -> Int -> Int -> Int -> Terreno
escolherTerreno ter@(t:ts:tss) seed l ti | lter == 2 && (mod (valorAlea) 2) == 0 = t
                                         | lter == 2 && (mod (valorAlea) 2) /= 0 = ts
                                         | lter == 3 && (mod (valorAlea) 3) == 0 = t
                                         | lter == 3 && (mod (valorAlea) 4) == 0 = ts
                                         | otherwise = head tss
      where lter = length ter
            n = (seed*ti*(seed-(l*ti))) `div` (seed+(ti*l))
            tam = (seed `div` 3)+l+ti
            pos = (seed `div` 4)+(l `div` 2)
            valorAlea = (!!) (listaAleatorios n tam) pos


-- | Funcao que gera pseudo-aleatoriamente a lista de obstaculos validos para a linha do mapa
gerarObstaculos :: Int -> (Terreno, [Obstaculo]) -> Int -> [Obstaculo]
gerarObstaculos l (t, obs) seed | obs == [] = gerarObstaculos l (t,(eo:[])) seed
                                | obs /= [] && l > lobs = gerarObstaculos l (t,((++) obs (eo:[]))) seed
                                | otherwise = ((++) obs [])
                  where lobs = length obs
                        eo = escolherObstaculos l (t, pov) seed
                        pov = proximosObstaculosValidos l (t, obs)

-- | Funcao que escolhe pseudo-aleatoriamente um elemento da lista de obstaculos para um dado terreno
escolherObstaculos :: Int -> (Terreno, [Obstaculo]) -> Int -> Obstaculo
escolherObstaculos l (t,[]) seed | t == (Rio 0) = (escolherObstaculos l (t,[Nenhum, Tronco]) seed)
                                 | t == (Estrada 0) = (escolherObstaculos l (t,[Nenhum, Carro]) seed)
                                 | t == (Relva) = (escolherObstaculos l (t,[Nenhum, Arvore]) seed)
escolherObstaculos l (t, obs@(o:os)) seed | lobs == 2 && (mod (valorAlea) 2) == 0 = (!!) os 0
                                          | otherwise = o
      where lobs = length obs
            n = ((seed-l)*(seed+l)) `div` (seed+lobs+l)
            tam = (seed `div` 3)+l+lobs
            pos = (seed `div` 4)+(l `div` 2)
            valorAlea = (!!) (listaAleatorios n tam) pos
