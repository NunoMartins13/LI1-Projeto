module Tarefa1_2022li1g001_Spec where

import LI12223
import Tarefa1_2022li1g001
import Test.HUnit

{- | Testes para a funcao mapaValido (junta os testes das funcoes auxiliares e faz dois testes gerais)
-}
testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test ["Teste 1" ~: True ~=? mapaValido (Mapa 12 [(Estrada 3,   [Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                                         (Estrada (-3),[Carro,Nenhum,Carro,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum]),
                                                                                         (Relva,       [Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore])]),
                                              
                                              "Teste 2" ~: False ~=? mapaValido (Mapa 12 [(Estrada 3,[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                                          (Rio (-3),[Carro,Nenhum,Carro,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum]),
                                                                                          (Relva,[Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore])]),
                                             
                                              testsObstaculoValido, testsRioContiguoValido, testsTroncoValido, testsCarroValido, testsNenhumValido, testsLarguraValida, testsTerrenosContiguos
                                             ]
                                             

-- | Testes para a funcao obstaculoValido
testsObstaculoValido = TestLabel "Testes para Obstáculos Válidos" $ test ["Teste Carro em Relva" ~: False ~=? obstaculoValido (Mapa 4 [ (Estrada 3,   [Carro,Carro,Carro,Nenhum]),
                                                                                                                                        (Estrada (-3),[Carro,Nenhum,Carro,Carro]),
                                                                                                                                        (Relva,       [Arvore,Nenhum,Carro,Arvore])]),
                                                                         
                                                                          "Teste Tronco em Relva" ~: False ~=? obstaculoValido (Mapa 4 [ (Estrada 3,   [Carro,Carro,Carro,Nenhum]),
                                                                                                                                         (Estrada (-3),[Carro,Nenhum,Carro,Carro]),
                                                                                                                                         (Relva,       [Arvore,Nenhum,Tronco,Arvore])]),

                                                                          "Teste Arvore em Estrada" ~: False ~=? obstaculoValido (Mapa 4 [ (Estrada 3,   [Arvore,Carro,Carro,Nenhum]),
                                                                                                                                           (Estrada (-3),[Carro,Nenhum,Carro,Carro]),
                                                                                                                                           (Relva,       [Arvore,Nenhum,Arvore,Arvore])]),

                                                                          "Teste Tronco em Estrada" ~: False ~=? obstaculoValido (Mapa 4 [ (Estrada 3,   [Carro,Carro,Tronco,Nenhum]),
                                                                                                                                           (Estrada (-3),[Carro,Nenhum,Carro,Carro]),
                                                                                                                                           (Relva,       [Arvore,Nenhum,Arvore,Arvore])]),

                                                                          "Teste Arvore em Rio" ~: False ~=? obstaculoValido (Mapa 4 [ (Estrada 3,   [Carro,Carro,Carro,Nenhum]),
                                                                                                                                       (Rio (-3),    [Tronco,Nenhum,Tronco,Arvore]),
                                                                                                                                       (Relva,       [Arvore,Nenhum,Arvore,Arvore])]),

                                                                          "Teste Carro em Rio" ~: False ~=? obstaculoValido (Mapa 4 [  (Estrada 3,   [Carro,Carro,Carro,Nenhum]),
                                                                                                                                       (Rio (-3),    [Tronco,Nenhum,Tronco,Arvore]),
                                                                                                                                       (Relva,       [Arvore,Nenhum,Arvore,Arvore])])
                                                                          ]


-- | Testes para a funcao rioContiguoValido
testsRioContiguoValido = TestLabel "Testes para Rios Contiguos Validos" $ test ["Teste mesma direcao" ~: False ~=? rioContiguoValido (Mapa 4 [  (Rio 3, [Tronco,Tronco,Tronco,Nenhum]),
                                                                                                                                                (Rio 3, [Tronco,Nenhum,Tronco,Tronco]),
                                                                                                                                                (Relva, [Arvore,Nenhum,Arvore,Arvore])]),

                                                                                "Teste direcao diferente" ~: True ~=? rioContiguoValido (Mapa 4 [(Rio 3,    [Tronco,Tronco,Tronco,Nenhum]),
                                                                                                                                                 (Rio (-3), [Tronco,Nenhum,Tronco,Tronco]),
                                                                                                                                                 (Relva,    [Arvore,Nenhum,Arvore,Arvore])])
                                                                               ]

-- | Testes para a funcao troncoValido
testsTroncoValido = TestLabel "Testes comprimento do Tronco" $ test ["Teste comprimento >5" ~: False ~=? troncoValido (Mapa 7 [  (Rio 3,    [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum]),
                                                                                                                                 (Rio (-3), [Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum]),
                                                                                                                                 (Relva,    [Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore])]),
                                             
                                                                     "Teste comprimento <=5" ~:True ~=? troncoValido (Mapa 7 [  (Rio 3,    [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum]),
                                                                                                                                (Rio (-3), [Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
                                                                                                                                (Relva,    [Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore])])
                                                                    ]

-- | Testes para a funcao carroValido
testsCarroValido = TestLabel "Testes comprimento do Carro" $ test ["Teste comprimento >3" ~: False ~=? carroValido (Mapa 5 [ (Estrada 3,   [Carro,Carro,Carro,Carro,Nenhum]),
                                                                                                                             (Estrada (-3),[Carro,Nenhum,Carro,Carro,Carro]),
                                                                                                                             (Relva,       [Arvore,Nenhum,Arvore,Arvore,Arvore])]),
                                                                   
                                                                   "Teste comprimento <=3" ~: True ~=? carroValido (Mapa 5 [ (Estrada 3,   [Carro,Carro,Carro,Nenhum,Nenhum]),
                                                                                                                             (Estrada (-3),[Carro,Nenhum,Carro,Carro,Carro]),
                                                                                                                             (Relva,       [Arvore,Nenhum,Arvore,Arvore,Arvore])])
                                                                  ]

-- | Testes para a funcao nenhumValido
testsNenhumValido = TestLabel "Testes verifica existencia de Nenhum" $ test ["Teste linha sem Nenhum" ~: False ~=? nenhumValido (Mapa 3 [ (Estrada 3,   [Carro,Carro,Nenhum]),
                                                                                                                                          (Estrada (-3),[Carro,Carro,Carro]),
                                                                                                                                          (Relva,       [Arvore,Nenhum,Arvore])]),
                                                               
                                                                             "Teste linha com Nenhum" ~: True ~=? nenhumValido (Mapa 3 [ (Estrada 3,   [Carro,Carro,Nenhum]),
                                                                                                                                         (Estrada (-3),[Carro,Nenhum,Carro]),
                                                                                                                                         (Relva,       [Arvore,Nenhum,Arvore])])
                                                                            ]

-- | Testes para a funcao larguraValida
testsLarguraValida = TestLabel "Teste valida a largura" $ test ["Teste largura maior que lista de obstaculos" ~: False ~=? larguraValida (Mapa 4 [ (Estrada 3,   [Carro,Carro,Nenhum]),
                                                                                                                                                   (Estrada (-3),[Carro,Nenhum,Carro]),
                                                                                                                                                   (Relva,       [Arvore,Nenhum,Arvore])]),
                                                                
                                                                "Teste largura menor que lista de osbtaculos" ~: False ~=? larguraValida (Mapa 2 [ (Estrada 3,   [Carro,Carro,Nenhum]),
                                                                                                                                                   (Estrada (-3),[Carro,Nenhum,Carro]),
                                                                                                                                                   (Relva,       [Arvore,Nenhum,Arvore])]),
                                                                
                                                                "Teste largura igual a lista de obstaculos"   ~: True  ~=? larguraValida (Mapa 3 [ (Estrada 3,   [Carro,Carro,Nenhum]),
                                                                                                                                                   (Estrada (-3),[Carro,Nenhum,Carro]),
                                                                                                                                                   (Relva,       [Arvore,Nenhum,Arvore])])
                                                               ]

est1 = (Estrada 1,[Carro,Carro,Nenhum])
rio1 = (Rio 1,[Tronco,Tronco,Nenhum])
rio2 = (Rio (-1),[Tronco,Tronco,Nenhum])

-- | Testes para a funcao terrenosContiguos
testsTerrenosContiguos = TestLabel "Teste terrenos contiguos validos" $ test ["Teste relva >5" ~: False ~=? terrenosContiguos (Mapa 3 [ (Relva,[Carro,Carro,Nenhum]),
                                                                                                                                        (Relva,[Carro,Nenhum,Carro]),
                                                                                                                                        (Relva,[Arvore,Nenhum,Arvore]),
                                                                                                                                        (Relva,[Arvore,Nenhum,Arvore]),
                                                                                                                                        (Relva,[Arvore,Nenhum,Arvore]),
                                                                                                                                        (Relva,[Arvore,Nenhum,Arvore])]),
                              
                                                                             "Teste relva <=5" ~: True ~=? terrenosContiguos  (Mapa 3 [ (Relva,[Carro,Carro,Nenhum]),
                                                                                                                                        (Relva,[Carro,Nenhum,Carro]),
                                                                                                                                        (Relva,[Arvore,Nenhum,Arvore]),
                                                                                                                                        (Relva,[Arvore,Nenhum,Arvore])]),
                                                                             
                                                                             "Teste estrada >5" ~: False ~=?  terrenosContiguos  (Mapa 3 [est1,est1,est1,est1,est1,est1]),

                                                                             "Teste estrada <=5" ~: True ~=? terrenosContiguos  (Mapa 3 [est1,est1,est1,est1,est1]),

                                                                             "Teste rio >4" ~: False ~=? terrenosContiguos (Mapa 3 [rio1,rio2,rio1,rio2,rio1]),

                                                                             "Teste rio >=4" ~: True ~=? terrenosContiguos (Mapa 3 [rio1,rio2,rio1,rio2])

                                                                             ]

                                                                            
                                                                                                                                        