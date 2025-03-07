module Tarefa4_2022li1g001_Spec where

import LI12223
import Tarefa4_2022li1g001
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste jogdor fora do mapa (limite inferior)" ~: True ~=? jogoTerminou (Jogo (Jogador (0,1)) (Mapa 3 [est1])),
 
                                              "Teste jogador fora do mapa (limite superior)" ~: True ~=? jogoTerminou (Jogo (Jogador (0,-1)) (Mapa 3 [est1])),

                                              "Teste jogador fora do mapa (limite esquerdo)" ~: True ~=? jogoTerminou (Jogo (Jogador (-1,0)) (Mapa 3 [est1])),

                                              "Teste jogador fora do mapa (limite direito)" ~: True ~=? jogoTerminou (Jogo (Jogador (1,0)) (Mapa 3 [est1])),
                                              
                                              "Teste jogador na agua" ~: True ~=? jogoTerminou (Jogo jog1 (Mapa 3 [rio1])),

                                              "Teste jogador debaixo de carro" ~: True ~=? jogoTerminou (Jogo jog1 (Mapa 3 [est1])),

                                              "Teste jogador no rio em cima de tronco" ~: False ~=? jogoTerminou (Jogo jog3 (Mapa 3 [est1,rio1])),

                                              "Teste jogador na estrada" ~: False ~=? jogoTerminou (Jogo jog2 (Mapa 3 [rio1,est1])),

                                              "Teste jogador na relva" ~: False ~=? jogoTerminou (Jogo jog3 (Mapa 3 [rio1,rel1]))                                         

                                             ]


est1 = (Estrada 1,[Carro,Carro,Nenhum])
rio1 = (Rio 1,[Nenhum,Tronco,Nenhum])
rel1 = (Relva,[Nenhum,Nenhum,Nenhum])
jog1 = Jogador (0,0)
jog2 = Jogador (2,1)
jog3 = Jogador (1,1)
