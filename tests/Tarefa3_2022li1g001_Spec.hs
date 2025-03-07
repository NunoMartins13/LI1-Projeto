module Tarefa3_2022li1g001_Spec where

import LI12223
import Tarefa3_2022li1g001
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste simples (Move Cima)" ~: (Jogo jog1 (Mapa 3 [rel1,rel1])) ~=? animaJogo (Jogo jog2 (Mapa 3 [rel1,rel1])) (Move Cima),

                                              "Teste simples (Move Baixo)" ~: (Jogo jog2 (Mapa 3 [rel1,rel1])) ~=? animaJogo (Jogo jog1 (Mapa 3 [rel1,rel1])) (Move Baixo),

                                              "Teste simples (Move Direita)" ~: (Jogo jog4 (Mapa 3 [rel1,rel1])) ~=? animaJogo (Jogo jog3 (Mapa 3 [rel1,rel1])) (Move Direita),

                                              "Teste simples (Move Esquerda)" ~: (Jogo jog3 (Mapa 3 [rel1,rel1])) ~=? animaJogo (Jogo jog4 (Mapa 3 [rel1,rel1])) (Move Esquerda),

                                              "Teste jogador na linha do topo do mapa com (Move Cima)" ~: (Jogo jog1 (Mapa 3 [rel1,rel1])) ~=? animaJogo (Jogo jog1 (Mapa 3 [rel1,rel1])) (Move Cima),
                                              
                                              "Teste jogador no limite esquerdo do mapa com (Move Esquerda)" ~: (Jogo jog1 (Mapa 3 [rel1,rel1])) ~=? animaJogo (Jogo jog1 (Mapa 3 [rel1,rel1])) (Move Esquerda),

                                              "Teste jogador no limite de baixo do mapa com (Move Baixo)" ~: (Jogo jog2 (Mapa 3 [rel1,rel1])) ~=? animaJogo (Jogo jog2 (Mapa 3 [rel1,rel1])) (Move Baixo),

                                              "Teste jogador no limite direito do mapa com (Move Direita)" ~: (Jogo jog4 (Mapa 3 [rel1,rel1])) ~=? animaJogo (Jogo jog4 (Mapa 3 [rel1,rel1])) (Move Direita),

                                              "Teste movimento de rio da equerda para a direita" ~: (Jogo jog1 (Mapa 3 [rel1,rio1'])) ~=? animaJogo (Jogo jog1 (Mapa 3 [rel1,rio1])) Parado,

                                              "Teste movimento de rio da direita para a esquerda" ~: (Jogo jog1 (Mapa 3 [rel1,rio2'])) ~=? animaJogo (Jogo jog1 (Mapa 3 [rel1,rio2])) Parado,

                                              "Teste movimento de estrada da esquerda para a direita" ~: (Jogo jog1 (Mapa 3 [rel1,est1'])) ~=? animaJogo (Jogo jog1 (Mapa 3 [rel1,est1])) Parado,

                                              "Teste movimento de estrada da direita para a esquerda" ~: (Jogo jog1 (Mapa 3 [rel1,est2'])) ~=? animaJogo (Jogo jog1 (Mapa 3 [rel1,est2])) Parado,

                                              "Teste jogador Parado em cima de tronco esq->dir" ~: (Jogo jog3 (Mapa 3 [rio1',rel1])) ~=? animaJogo (Jogo jog1 (Mapa 3 [rio1,rel1])) Parado,

                                              "Teste jogador Parado em cima de troco dir->esq" ~: (Jogo jog1 (Mapa 3 [rio3,rel1])) ~=? animaJogo (Jogo jog4 (Mapa 3 [rio2',rel1])) Parado,

                                              "Teste (Move Cima) de relva para tronco" ~: (Jogo jog4 (Mapa 3 [rio1',rel1])) ~=? animaJogo (Jogo jog5 (Mapa 3 [rio1,rel1])) (Move Cima),

                                              "Teste (Move Baixo) de relva para tronco" ~: (Jogo jog5 (Mapa 3 [rel1,rio1'])) ~=? animaJogo (Jogo jog1 (Mapa 3 [rel1,rio1])) (Move Baixo),

                                              "Teste Arvore Cima" ~: (Jogo jog2 (Mapa 3 [rel2,rel1])) ~=? animaJogo (Jogo jog2 (Mapa 3 [rel2,rel1])) (Move Cima),

                                              "Testa Arvore Baixo" ~: (Jogo jog2 (Mapa 3 [rel1,rel2])) ~=? animaJogo (Jogo jog2 (Mapa 3 [rel1,rel2])) (Move Baixo),

                                              "Testa Arvore Direita" ~: (Jogo jog5 (Mapa 3 [rel1,rel2])) ~=? animaJogo (Jogo jog5 (Mapa 3 [rel1,rel2])) (Move Direita),

                                              "Testa Arvore Esquerda" ~: (Jogo jog5 (Mapa 3 [rel1,rel2])) ~=? animaJogo (Jogo jog5 (Mapa 3 [rel1,rel2])) (Move Esquerda)


                                             ]



est1 = (Estrada 1,[Carro,Carro,Nenhum])
est1'= (Estrada 1,[Nenhum,Carro,Carro])
est2 = (Estrada (-2),[Carro,Carro,Nenhum])
est2'= (Estrada (-2),[Nenhum,Carro,Carro])
rio1 = (Rio 1,[Tronco,Tronco,Nenhum])
rio1'= (Rio 1,[Nenhum,Tronco,Tronco])
rio2 = (Rio (-2),[Tronco,Tronco,Nenhum])
rio2'= (Rio (-2),[Nenhum,Tronco,Tronco])
rio3 = (Rio (-2),[Tronco,Nenhum,Tronco])
rel1 = (Relva,[Nenhum,Nenhum,Nenhum])
rel2 = (Relva,[Arvore,Nenhum,Arvore])

jog1 = Jogador (0,0)
jog2 = Jogador (0,1)
jog3 = Jogador (1,0)
jog4 = Jogador (2,0)
jog5 = Jogador (1,1)

