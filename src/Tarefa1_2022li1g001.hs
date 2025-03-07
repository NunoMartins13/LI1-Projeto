{- |
Module      : Tarefa1_2022li1g001
Description : Validação de um mapa
Copyright   : Carlos Eduardo Martins de Sá Fernandes <a100890@alunos.uminho.pt>
              Nuno de Freitas Machado Martins <a96227@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g001 where

import LI12223

import Data.List

{- | Funcao que verifica se um mapa é ou não válido conforme as restrições
-}
mapaValido :: Mapa -> Bool
mapaValido m = (obstaculoValido m) && (rioContiguoValido m) && (troncoValido m) && (carroValido m) && (larguraValida m) && (terrenosContiguos m)


{- | Funcoes de restricao para mapaValido
-}
-- | Verifica se exitem obstaculos em terrenos impróprios.
obstaculoValido :: Mapa -> Bool  
obstaculoValido (Mapa l []) = True 
obstaculoValido (Mapa l ((t,(o:os)):ls)) = case t of (Rio _)     -> if ((Arvore `elem` (o:os)) || (Carro `elem` (o:os)))  then False 
                                                                                                                          else True && obstaculoValido (Mapa l ls)
                                           
                                                     (Estrada _) -> if ((Arvore `elem` (o:os)) || (Tronco `elem` (o:os))) then False 
                                                                                                                          else True && obstaculoValido (Mapa l ls)

                                                     (Relva)     -> if ((Carro `elem` (o:os)) || (Tronco `elem` (o:os)))  then False 
                                                                                                                          else True && obstaculoValido (Mapa l ls)


-- | Verifica se rios contiguos possuem direcoes opostas
rioContiguoValido :: Mapa -> Bool
rioContiguoValido (Mapa l []) = True 
rioContiguoValido (Mapa l ((Rio v1,_):(Rio v2,_):ls)) = ((v1 > 0 && v2 < 0) || (v1 < 0 && v2 > 0)) && rioContiguoValido (Mapa l ls)
rioContiguoValido (Mapa l ((t,_):ls)) = True && rioContiguoValido (Mapa l ls)


-- | Verifica se o comprimento do Tronco e valido (<=5)
troncoValido :: Mapa -> Bool
troncoValido (Mapa l []) = True 
troncoValido (Mapa l ((t,obs):ls)) = ((comprimentoValido obs Tronco) <= 5) && troncoValido (Mapa l ls)  


-- | Verifica se o comprimento do Carro e valido (<=3)
carroValido :: Mapa -> Bool
carroValido (Mapa l []) = True 
carroValido (Mapa l ((t,obs):ls)) = ((comprimentoValido obs Carro) <= 3) && carroValido (Mapa l ls)


-- | Verifica se existe pelo menos um espaço livre numa linha
nenhumValido :: Mapa -> Bool
nenhumValido (Mapa l []) = True
nenhumValido (Mapa l ((t,obs):ls)) = (Nenhum `elem` obs) && nenhumValido (Mapa l ls)


-- | Verifica se que o comprimento da lista de obstaculos é corresponde à largura do mapa
larguraValida :: Mapa -> Bool
larguraValida (Mapa l []) = True
larguraValida (Mapa l ((t,obs):ls)) = (l == length obs) && larguraValida (Mapa l ls)


-- | Verifca que não existem mais de 4 rios, nem 5 estradas ou relvas contiguamente
terrenosContiguos :: Mapa -> Bool
terrenosContiguos (Mapa l []) = True
terrenosContiguos (Mapa l lst@((t,obs):ls)) = (comprimentoValido (remVel(listaTerrenos lst)) "rel") <= 5
                                            &&(comprimentoValido (remVel(listaTerrenos lst)) "est") <= 5
                                            &&(comprimentoValido (remVel(listaTerrenos lst)) "rio") <= 4





{- |Funcoes auxiliares 
-}

-- | Funcao complementar da maxSeq
comprimentoValido :: Eq a => [a] -> a -> Int
comprimentoValido obs x = maxSeq obs x 0

-- | Funcao que devolve o tamanho da maior sequencia de um elemento numa lista
maxSeq :: Eq a => [a] -> a -> Int -> Int  
maxSeq [] x s = s  
maxSeq obs x s = maxSeq (drop (l+1) obs) x s'  
                        where l = count obs x 
                              s' = if l>s then l else s 

-- ! Funcao auxiliar da maxSeq que conta os elementos iguais de uma lista
count :: Eq a => [a] -> a -> Int 
count [] _ = 0  
count (h:t) x | x == h = 1 + count t x
              | otherwise = 0

-- | Funcao que recebe a lista do mapa e devolve uma lista apenas com os tipos de terreno
listaTerrenos :: [(Terreno, [Obstaculo])] -> [Terreno]
listaTerrenos [] = []
listaTerrenos ((t,obs):ls) = (t:listaTerrenos ls)

-- | Funcao que recebe uma lista de terrenos e devolve uma lista com as strings correspondentes, util para remover o fator velocidade
remVel :: [Terreno] -> [String]
remVel [] = []
remVel (x:xs)=
             case x of (Rio _) -> "rio":(remVel xs)
                       (Estrada _) -> "est":(remVel xs)
                       Relva -> "rel":(remVel xs)

-- | Funcao que recebe a lista do mapa e devolve uma lista com as listas de obstaculos
listaObstaculos :: [(Terreno,[Obstaculo])] -> [[Obstaculo]]
listaObstaculos [] = []
listaObstaculos ((t,obs):ls) = (obs:(listaObstaculos ls))