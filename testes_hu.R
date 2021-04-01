

###########TESTES DE CORRELACAO###################

##################teste cor##############
###valor-p < 0,05 - Rejeita Ho
###valor-p > 0,05 - Aceita Ho
##Ho: nao h? correlacao
##H1: h? correlacao

bd <- read.csv("~/projetos_r/novo_huu"); head(bd)

###!!!correlacao teste e do pairs ta dando diferente

##shaps
cor.test(bd$shaps, bd$acips_total) #p = 1,302e < 0,05
names(cor.test(bd$shaps, bd$acips_total))#o que ele calcula

cbind(cor.test(bd$shaps, bd$acips_total)$estimate, cor.test(bd$shaps, bd$acips_total)$p.value)
##cor, valor-p

cor.test(bd$shaps, bd$teps_total) #p = 4,069e < 0,05
cor.test(bd$shaps, bd$hads_total) #p = 7,623e < 0,05
cor.test(bd$shaps, bd$neuroticismo) #p = 0,4537 > 0,05
cor.test(bd$shaps, bd$PCR) #p = 0,1559 > 0,05
cor.test(bd$shaps, bd$CRH) #p = 0,5385 > 0,05
cor.test(bd$shaps, bd$cortisol) #p = 0,7092
cor.test(bd$shaps, bd$idade) #p = 0,9355 
cor.test(bd$shaps, bd$tempo_casado) #p = 0,76
cor.test(bd$shaps, bd$num_filhos) #p = 0,7682
cor.test(bd$shaps, bd$renda_familia) #p = 0,5127
cor.test(bd$shaps, bd$peso) #p = 0,3912
cor.test(bd$shaps, bd$altura) #p = 0,1556

##acips
cor.test(bd$acips_total, bd$teps_total) #p = 1,276e
cor.test(bd$acips_total, bd$hads_total) #p = 0,0002
cor.test(bd$acips_total, bd$neuroticismo) #p = 0,5669
cor.test(bd$acips_total, bd$PCR) #p = 0,2426
cor.test(bd$acips_total, bd$CRH) #p = 0,7678
cor.test(bd$acips_total, bd$cortisol) #p = 0,4447
cor.test(bd$acips_total, bd$idade) #p = 0,4651 
cor.test(bd$acips_total, bd$tempo_casado) #p = 0,7058
cor.test(bd$acips_total, bd$num_filhos) #p = 0,7682
cor.test(bd$acips_total, bd$renda_familia) #p = 0,6136
cor.test(bd$acips_total, bd$peso) #p = 0,5779
cor.test(bd$acips_total, bd$altura) #p = 0,9364

##teps
cor.test(bd$teps_total, bd$hads_total) #p = 0,0002
cor.test(bd$teps_total, bd$neuroticismo) #p = 0,5228
cor.test(bd$teps_total, bd$PCR) #p = 0,3237
cor.test(bd$teps_total, bd$CRH) #p = 0,8887
cor.test(bd$teps_total, bd$cortisol) #p = 0,3436
cor.test(bd$teps_total, bd$idade) #p = 0,1533
cor.test(bd$teps_total, bd$tempo_casado) #p = 0,6978
cor.test(bd$teps_total, bd$num_filhos) #p = 0,8413
cor.test(bd$teps_total, bd$renda_familia) #p = 0,5949
cor.test(bd$teps_total, bd$peso) #p = 0,8894
cor.test(bd$teps_total, bd$altura) #p = 0,9984

##hads
cor.test(bd$hads_total, bd$neuroticismo) #p = 2,661e
cor.test(bd$hads_total, bd$PCR) #p = 0,828
cor.test(bd$hads_total, bd$CRH) #p = 0,801
cor.test(bd$hads_total, bd$cortisol) #p = 0,9444
cor.test(bd$hads_total, bd$idade) #p = 0,6881
cor.test(bd$hads_total, bd$tempo_casado) #p = 0,812
cor.test(bd$hads_total, bd$num_filhos) #p = 0,7881
cor.test(bd$hads_total, bd$renda_familia) #p = 0,4254
cor.test(bd$hads_total, bd$peso) #p = 0,1073
cor.test(bd$hads_total, bd$altura) #p = 0,4858

##neuroticismo
cor.test(bd$neuroticismo, bd$PCR) #p = 0,5197
cor.test(bd$neuroticismo, bd$CRH) #p = 0,6605
cor.test(bd$neuroticismo, bd$cortisol) #p = 0,5785
cor.test(bd$neuroticismo, bd$idade) #p = 0,1226
cor.test(bd$neuroticismo, bd$tempo_casado) #p = 0,2369
cor.test(bd$neuroticismo, bd$num_filhos) #p = 0,3281
cor.test(bd$neuroticismo, bd$renda_familia) #p = 0,22
cor.test(bd$neuroticismo, bd$peso) #p = 0,2595
cor.test(bd$neuroticismo, bd$altura) #p = 0,101

##pcr
cor.test(bd$PCR, bd$CRH) #p = 0,3318
cor.test(bd$PCR, bd$cortisol) #p = 0,0471
cor.test(bd$PCR, bd$idade) #p = 0,4733
cor.test(bd$PCR, bd$tempo_casado) #p = 0,5811
cor.test(bd$PCR, bd$num_filhos) #p = 0,67
cor.test(bd$PCR, bd$renda_familia) #p = 0,973
cor.test(bd$PCR, bd$peso) #p = 0,2849
cor.test(bd$PCR, bd$altura) #p = 0,7841

##crh
cor.test(bd$CRH, bd$cortisol) #p = 0,835
cor.test(bd$CRH, bd$idade) #p = 0,5346
cor.test(bd$CRH, bd$tempo_casado) #p = 0,8918
cor.test(bd$CRH, bd$num_filhos) #p = 0,9568
cor.test(bd$CRH, bd$renda_familia) #p = 0,2633
cor.test(bd$CRH, bd$peso) #p = 0,7024
cor.test(bd$CRH, bd$altura) #p = 0,84

##cortisol
cor.test(bd$cortisol, bd$idade) #p = 0,2683
cor.test(bd$cortisol, bd$tempo_casado) #p = 0,2638
cor.test(bd$cortisol, bd$num_filhos) #p = 0,5847
cor.test(bd$cortisol, bd$renda_familia) #p = 0,8742
cor.test(bd$cortisol, bd$peso) #p = 0,4182
cor.test(bd$cortisol, bd$altura) #p = 0,3933

#idade
cor.test(bd$idade, bd$tempo_casado) #p = 0,061
cor.test(bd$idade, bd$num_filhos) #p = 0,05118
cor.test(bd$idade, bd$renda_familia) #p = 0,4281
cor.test(bd$idade, bd$peso) #p = 0,9657
cor.test(bd$idade, bd$altura) #p = 0,008

#tempo_casado
cor.test(bd$tempo_casado, bd$num_filhos) #p = 6,946e
cor.test(bd$tempo_casado, bd$renda_familia) #p = 0,2579
cor.test(bd$tempo_casado, bd$peso) #p = 0,1411
cor.test(bd$tempo_casado, bd$altura) #p = 0,3478

#numero_filhos
cor.test(bd$num_filhos, bd$renda_familia) #p = 0,7107
cor.test(bd$num_filhos, bd$peso) #p = 0,5921
cor.test(bd$num_filhos, bd$altura) #p = 0,7373

#renda
cor.test(bd$renda_familia, bd$peso) #p = 0,7628
cor.test(bd$renda_familia, bd$altura) #p = 0,7886

#altura
cor.test(bd$peso, bd$altura) #p = 0,0307

#hemacias
cor.test(bd$hemacias, bd$shaps) #p=0,9984
cor.test(bd$hemacias, bd$acips_total) #p = 0,7997
cor.test(bd$hemacias, bd$teps_total) #p = 0,9299
cor.test(bd$hemacias, bd$hads_total) #p = 0,8345
cor.test(bd$hemacias, bd$neuroticismo) #p = 0,3294
cor.test(bd$hemacias, bd$PCR) #p = 0,4168
cor.test(bd$hemacias, bd$CRH) #p = 0,05332
cor.test(bd$hemacias, bd$cortisol) #p = 0,985
cor.test(bd$hemacias, bd$idade) #p = 0,1949
cor.test(bd$hemacias, bd$tempo_casado) #p = 0,3001
cor.test(bd$hemacias, bd$num_filhos) #p = 0,3224
cor.test(bd$hemacias, bd$renda_familia) #p = 0,8331
cor.test(bd$hemacias, bd$peso) #p = 0,4169
cor.test(bd$hemacias, bd$altura) #p = 0,7599

cor.test(bd$hemacias, bd$hemoglobina) #p=1,515e
cor.test(bd$hemacias, bd$hematocrito) #p = 1,787e
cor.test(bd$hemacias, bd$volumeglob) #p = 0,0004
cor.test(bd$hemacias, bd$hemoglobina_m) #p = 3,577e
cor.test(bd$hemacias, bd$hemoglobina_concent) #p = 0,031
cor.test(bd$hemacias, bd$rdw) #p = 0,1944
cor.test(bd$hemacias, bd$leucocitos) #p=0,0433
cor.test(bd$hemacias, bd$segmentados) #p = 0,3949
cor.test(bd$hemacias, bd$segmentados_m) #p = 0,3868
cor.test(bd$hemacias, bd$bastonados) #p = 0,9585
cor.test(bd$hemacias, bd$linfocitos) #p = 0,2385
cor.test(bd$hemacias, bd$linfocitos_m) #p = 0,02838
cor.test(bd$hemacias, bd$monocitos) #p = 0,2379
cor.test(bd$hemacias, bd$monocitos_m) #p = 0,4438
cor.test(bd$hemacias, bd$eosinofilos) #p = 0,7389
cor.test(bd$hemacias, bd$eosinofilos_m) #p = 0,7812
cor.test(bd$hemacias, bd$basofilos) #p = 0,8618
cor.test(bd$hemacias, bd$basofilos_m) #p = 0,5594
cor.test(bd$hemacias, bd$plaquetas) #p = 0,7974
cor.test(bd$hemacias, bd$plaquetas_vol) #p = 0,4853

#hemoglobina
cor.test(bd$hemoglobina, bd$shaps) #p=0,7394
cor.test(bd$hemoglobina, bd$acips_total) #p = 0,9677
cor.test(bd$hemoglobina, bd$teps_total) #p = 0,7632
cor.test(bd$hemoglobina, bd$hads_total) #p = 0,6756
cor.test(bd$hemoglobina, bd$neuroticismo) #p = 0,8007
cor.test(bd$hemoglobina, bd$PCR) #p = 0,5773
cor.test(bd$hemoglobina, bd$CRH) #p = 0,3956
cor.test(bd$hemoglobina, bd$cortisol) #p = 0,3621
cor.test(bd$hemoglobina, bd$idade) #p = 0,0009
cor.test(bd$hemoglobina, bd$tempo_casado) #p = 0,2932
cor.test(bd$hemoglobina, bd$num_filhos) #p = 0,662
cor.test(bd$hemoglobina, bd$renda_familia) #p = 0,8559
cor.test(bd$hemoglobina, bd$peso) #p = 0,496
cor.test(bd$hemoglobina, bd$altura) #p = 0,04276

cor.test(bd$hemoglobina, bd$hematocrito) #p = 2,2e
cor.test(bd$hemoglobina, bd$volumeglob) #p = 0,1779
cor.test(bd$hemoglobina, bd$hemoglobina_m) #p = 0,087
cor.test(bd$hemoglobina, bd$hemoglobina_concent) #p = 0,2873
cor.test(bd$hemoglobina, bd$rdw) #p = 0,8733
cor.test(bd$hemoglobina, bd$leucocitos) #p=0,05468
cor.test(bd$hemoglobina, bd$segmentados) #p = 0,0772
cor.test(bd$hemoglobina, bd$segmentados_m) #p = 0,9986
cor.test(bd$hemoglobina, bd$bastonados) #p = 0,5173
cor.test(bd$hemoglobina, bd$linfocitos) #p = 0,0652
cor.test(bd$hemoglobina, bd$linfocitos_m) #p = 0,0055
cor.test(bd$hemoglobina, bd$monocitos) #p = 0,092
cor.test(bd$hemoglobina, bd$monocitos_m) #p = 0,8531
cor.test(bd$hemoglobina, bd$eosinofilos) #p = 0,3777
cor.test(bd$hemoglobina, bd$eosinofilos_m) #p = 0,1619
cor.test(bd$hemoglobina, bd$basofilos) #p = 0,3502
cor.test(bd$hemoglobina, bd$basofilos_m) #p = 0,1162
cor.test(bd$hemoglobina, bd$plaquetas) #p = 0,0305
cor.test(bd$hemoglobina, bd$plaquetas_vol) #p = 0,3399

#hematocrito
cor.test(bd$hematocrito, bd$shaps) #p=0,8086
cor.test(bd$hematocrito, bd$acips_total) #p = 0,803
cor.test(bd$hematocrito, bd$teps_total) #p = 0,8001
cor.test(bd$hematocrito, bd$hads_total) #p = 0,7018
cor.test(bd$hematocrito, bd$neuroticismo) #p = 0,9329
cor.test(bd$hematocrito, bd$PCR) #p = 0,2702
cor.test(bd$hematocrito, bd$CRH) #p = 0,2054
cor.test(bd$hematocrito, bd$cortisol) #p = 0,1713
cor.test(bd$hematocrito, bd$idade) #p = 0,002
cor.test(bd$hematocrito, bd$tempo_casado) #p = 0,1857
cor.test(bd$hematocrito, bd$num_filhos) #p = 0,852
cor.test(bd$hematocrito, bd$renda_familia) #p = 0,9903
cor.test(bd$hematocrito, bd$peso) #p = 0,4696
cor.test(bd$hematocrito, bd$altura) #p = 0,1239

cor.test(bd$hematocrito, bd$volumeglob) #p = 0,2808
cor.test(bd$hematocrito, bd$hemoglobina_m) #p = 0,9292
cor.test(bd$hematocrito, bd$hemoglobina_concent) #p = 0,038
cor.test(bd$hematocrito, bd$rdw) #p = 0,1705
cor.test(bd$hematocrito, bd$leucocitos) #p=0,015
cor.test(bd$hematocrito, bd$segmentados) #p = 0,3456
cor.test(bd$hematocrito, bd$segmentados_m) #p = 0,3247
cor.test(bd$hematocrito, bd$bastonados) #p = 0,4316
cor.test(bd$hematocrito, bd$linfocitos) #p = 0,4021
cor.test(bd$hematocrito, bd$linfocitos_m) #p = 0,024
cor.test(bd$hematocrito, bd$monocitos) #p = 0,1066
cor.test(bd$hematocrito, bd$monocitos_m) #p = 0,5934
cor.test(bd$hematocrito, bd$eosinofilos) #p = 0,0804
cor.test(bd$hematocrito, bd$eosinofilos_m) #p = 0,0166
cor.test(bd$hematocrito, bd$basofilos) #p = 0,4312
cor.test(bd$hematocrito, bd$basofilos_m) #p = 0,0838
cor.test(bd$hematocrito, bd$plaquetas) #p = 0,2499
cor.test(bd$hematocrito, bd$plaquetas_vol) #p = 0,1186

#volume globular
cor.test(bd$volumeglob, bd$shaps) #p=0,5516
cor.test(bd$volumeglob, bd$acips_total) #p = 0,3414
cor.test(bd$volumeglob, bd$teps_total) #p = 0,6497
cor.test(bd$volumeglob, bd$hads_total) #p = 0,3395
cor.test(bd$volumeglob, bd$neuroticismo) #p = 0,1847
cor.test(bd$volumeglob, bd$PCR) #p = 0,8034
cor.test(bd$volumeglob, bd$CRH) #p = 0,2865
cor.test(bd$volumeglob, bd$cortisol) #p = 0,0511
cor.test(bd$volumeglob, bd$idade) #p = 0,0506
cor.test(bd$volumeglob, bd$tempo_casado) #p = 0,778
cor.test(bd$volumeglob, bd$num_filhos) #p = 0,1277
cor.test(bd$volumeglob, bd$renda_familia) #p = 0,6709
cor.test(bd$volumeglob, bd$peso) #p = 0,9362
cor.test(bd$volumeglob, bd$altura) #p = 0,0018

cor.test(bd$volumeglob, bd$hemoglobina_m) #p = 1,113e
cor.test(bd$volumeglob, bd$hemoglobina_concent) #p = 0,6909
cor.test(bd$volumeglob, bd$rdw) #p = 0,8928
cor.test(bd$volumeglob, bd$leucocitos) #p=0,9425
cor.test(bd$volumeglob, bd$segmentados) #p = 0,783
cor.test(bd$volumeglob, bd$segmentados_m) #p = 0,9231
cor.test(bd$volumeglob, bd$bastonados) #p = 0,2662
cor.test(bd$volumeglob, bd$linfocitos) #p = 0,6395
cor.test(bd$volumeglob, bd$linfocitos_m) #p = 0,7104
cor.test(bd$volumeglob, bd$monocitos) #p = 0,84
cor.test(bd$volumeglob, bd$monocitos_m) #p = 0,6495
cor.test(bd$volumeglob, bd$eosinofilos) #p = 0,0012
cor.test(bd$volumeglob, bd$eosinofilos_m) #p = 0,0028
cor.test(bd$volumeglob, bd$basofilos) #p = 0,1356
cor.test(bd$volumeglob, bd$basofilos_m) #p = 0,1092
cor.test(bd$volumeglob, bd$plaquetas) #p = 0,2146
cor.test(bd$volumeglob, bd$plaquetas_vol) #p = 0,3456

#hemoglobina media
cor.test(bd$hemoglobina_m, bd$shaps) #p=0,5547
cor.test(bd$hemoglobina_m, bd$acips_total) #p = 0,6611
cor.test(bd$hemoglobina_m, bd$teps_total) #p = 0,6442
cor.test(bd$hemoglobina_m, bd$hads_total) #p = 0,4314
cor.test(bd$hemoglobina_m, bd$neuroticismo) #p = 0,1558
cor.test(bd$hemoglobina_m, bd$PCR) #p = 0,6437
cor.test(bd$hemoglobina_m, bd$CRH) #p = 0,2008
cor.test(bd$hemoglobina_m, bd$cortisol) #p = 0,3011
cor.test(bd$hemoglobina_m, bd$idade) #p = 0,0826
cor.test(bd$hemoglobina_m, bd$tempo_casado) #p = 0,9079
cor.test(bd$hemoglobina_m, bd$num_filhos) #p = 0,352
cor.test(bd$hemoglobina_m, bd$renda_familia) #p = 0,8431
cor.test(bd$hemoglobina_m, bd$peso) #p = 0,871
cor.test(bd$hemoglobina_m, bd$altura) #p = 0,0035

cor.test(bd$hemoglobina_m, bd$hemoglobina_concent) #p = 4,153e
cor.test(bd$hemoglobina_m, bd$rdw) #p = 0,228
cor.test(bd$hemoglobina_m, bd$leucocitos) #p=0,4755
cor.test(bd$hemoglobina_m, bd$segmentados) #p = 0,3042
cor.test(bd$hemoglobina_m, bd$segmentados_m) #p = 0,2294
cor.test(bd$hemoglobina_m, bd$bastonados) #p = 0,4783
cor.test(bd$hemoglobina_m, bd$linfocitos) #p = 0,5485
cor.test(bd$hemoglobina_m, bd$linfocitos_m) #p = 0,978
cor.test(bd$hemoglobina_m, bd$monocitos) #p = 0,845
cor.test(bd$hemoglobina_m, bd$monocitos_m) #p = 0,2248
cor.test(bd$hemoglobina_m, bd$eosinofilos) #p = 0,1137
cor.test(bd$hemoglobina_m, bd$eosinofilos_m) #p = 0,2056
cor.test(bd$hemoglobina_m, bd$basofilos) #p = 0,1697
cor.test(bd$hemoglobina_m, bd$basofilos_m) #p = 0,2753
cor.test(bd$hemoglobina_m, bd$plaquetas) #p = 0,036
cor.test(bd$hemoglobina_m, bd$plaquetas_vol) #p = 0,939

#hemoglobina concentrada
cor.test(bd$hemoglobina_concent, bd$shaps) #p=0,7955
cor.test(bd$hemoglobina_concent, bd$acips_total) #p = 0,5727
cor.test(bd$hemoglobina_concent, bd$teps_total) #p = 0,9477
cor.test(bd$hemoglobina_concent, bd$hads_total) #p = 0,8879
cor.test(bd$hemoglobina_concent, bd$neuroticismo) #p = 0,4735
cor.test(bd$hemoglobina_concent, bd$PCR) #p = 0,2521
cor.test(bd$hemoglobina_concent, bd$CRH) #p = 0,4047
cor.test(bd$hemoglobina_concent, bd$cortisol) #p = 0,2531
cor.test(bd$hemoglobina_concent, bd$idade) #p = 0,8053
cor.test(bd$hemoglobina_concent, bd$tempo_casado) #p = 0,5234
cor.test(bd$hemoglobina_concent, bd$num_filhos) #p = 0,5802
cor.test(bd$hemoglobina_concent, bd$renda_familia) #p = 0,7828
cor.test(bd$hemoglobina_concent, bd$peso) #p = 0,7787
cor.test(bd$hemoglobina_concent, bd$altura) #p = 0,4945

cor.test(bd$hemoglobina_concent, bd$rdw) #p = 0,005
cor.test(bd$hemoglobina_concent, bd$leucocitos) #p=0,1997
cor.test(bd$hemoglobina_concent, bd$segmentados) #p = 0,1239
cor.test(bd$hemoglobina_concent, bd$segmentados_m) #p = 0,026
cor.test(bd$hemoglobina_concent, bd$bastonados) #p = 0,6943
cor.test(bd$hemoglobina_concent, bd$linfocitos) #p = 0,049
cor.test(bd$hemoglobina_concent, bd$linfocitos_m) #p = 0,5052
cor.test(bd$hemoglobina_concent, bd$monocitos) #p = 0,8949
cor.test(bd$hemoglobina_concent, bd$monocitos_m) #p = 0,087
cor.test(bd$hemoglobina_concent, bd$eosinofilos) #p = 0,048
cor.test(bd$hemoglobina_concent, bd$eosinofilos_m) #p = 0,0244
cor.test(bd$hemoglobina_concent, bd$basofilos) #p = 0,8062
cor.test(bd$hemoglobina_concent, bd$basofilos_m) #p = 0,6394
cor.test(bd$hemoglobina_concent, bd$plaquetas) #p = 0,0448
cor.test(bd$hemoglobina_concent, bd$plaquetas_vol) #p = 0,1489

#rdw
cor.test(bd$rdw, bd$shaps) #p=0,1334
cor.test(bd$rdw, bd$acips_total) #p = 0,4833
cor.test(bd$rdw, bd$teps_total) #p = 0,6599
cor.test(bd$rdw, bd$hads_total) #p = 0,4022
cor.test(bd$rdw, bd$neuroticismo) #p = 0,5773
cor.test(bd$rdw, bd$PCR) #p = 0,4987
cor.test(bd$rdw, bd$CRH) #p = 0,037
cor.test(bd$rdw, bd$cortisol) #p = 0,6995
cor.test(bd$rdw, bd$idade) #p = 0,0671
cor.test(bd$rdw, bd$tempo_casado) #p = 0,433
cor.test(bd$rdw, bd$num_filhos) #p = 0,385
cor.test(bd$rdw, bd$renda_familia) #p = 0,0672
cor.test(bd$rdw, bd$peso) #p = 0,0097
cor.test(bd$rdw, bd$altura) #p = 0,292

cor.test(bd$rdw, bd$leucocitos) #p=0,1671
cor.test(bd$rdw, bd$segmentados) #p = 0,4464
cor.test(bd$rdw, bd$segmentados_m) #p = 0,2244
cor.test(bd$rdw, bd$bastonados) #p = 0,7462
cor.test(bd$rdw, bd$linfocitos) #p = 0,8698
cor.test(bd$rdw, bd$linfocitos_m) #p = 0,3668
cor.test(bd$rdw, bd$monocitos) #p = 0,2999
cor.test(bd$rdw, bd$monocitos_m) #p = 0,0145
cor.test(bd$rdw, bd$eosinofilos) #p = 0,048
cor.test(bd$rdw, bd$eosinofilos_m) #p = 0,0659
cor.test(bd$rdw, bd$basofilos) #p = 0,4015
cor.test(bd$rdw, bd$basofilos_m) #p = 0,1646
cor.test(bd$rdw, bd$plaquetas) #p = 0,914
cor.test(bd$rdw, bd$plaquetas_vol) #p = 0,5678

#leucocitos
cor.test(bd$leucocitos, bd$shaps) #p=0,9593
cor.test(bd$leucocitos, bd$acips_total) #p = 0,2727
cor.test(bd$leucocitos, bd$teps_total) #p = 0,2081
cor.test(bd$leucocitos, bd$hads_total) #p = 0,6709
cor.test(bd$leucocitos, bd$neuroticismo) #p = 0,5342
cor.test(bd$leucocitos, bd$PCR) #p = 0,8802
cor.test(bd$leucocitos, bd$CRH) #p = 0,4489
cor.test(bd$leucocitos, bd$cortisol) #p = 0,9261
cor.test(bd$leucocitos, bd$idade) #p = 0,0001
cor.test(bd$leucocitos, bd$tempo_casado) #p = 0,3476
cor.test(bd$leucocitos, bd$num_filhos) #p = 0,9088
cor.test(bd$leucocitos, bd$renda_familia) #p = 0,5776
cor.test(bd$leucocitos, bd$peso) #p = 0,0999
cor.test(bd$leucocitos, bd$altura) #p = 0,0028

cor.test(bd$leucocitos, bd$segmentados) #p = 0,2717
cor.test(bd$leucocitos, bd$segmentados_m) #p = 3,698e
cor.test(bd$leucocitos, bd$bastonados) #p = 0,0397
cor.test(bd$leucocitos, bd$linfocitos) #p = 0,6017
cor.test(bd$leucocitos, bd$linfocitos_m) #p = 2,455e
cor.test(bd$leucocitos, bd$monocitos) #p = 0,0027
cor.test(bd$leucocitos, bd$monocitos_m) #p = 4,224e
cor.test(bd$leucocitos, bd$eosinofilos) #p = 0,9971
cor.test(bd$leucocitos, bd$eosinofilos_m) #p = 0,1036
cor.test(bd$leucocitos, bd$basofilos) #p = 0,213
cor.test(bd$leucocitos, bd$basofilos_m) #p = 0,414
cor.test(bd$leucocitos, bd$plaquetas) #p = 0,6379
cor.test(bd$leucocitos, bd$plaquetas_vol) #p = 0,5121

#segmentados
cor.test(bd$segmentados, bd$shaps) #p=0,0776
cor.test(bd$segmentados, bd$acips_total) #p = 0,4413
cor.test(bd$segmentados, bd$teps_total) #p = 0,0493
cor.test(bd$segmentados, bd$hads_total) #p = 0,6684
cor.test(bd$segmentados, bd$neuroticismo) #p = 0,3867
cor.test(bd$segmentados, bd$PCR) #p = 0,6542
cor.test(bd$segmentados, bd$CRH) #p = 0,2391
cor.test(bd$segmentados, bd$cortisol) #p = 0,9468
cor.test(bd$segmentados, bd$idade) #p = 0,8852
cor.test(bd$segmentados, bd$tempo_casado) #p = 0,4829
cor.test(bd$segmentados, bd$num_filhos) #p = 0,7381
cor.test(bd$segmentados, bd$renda_familia) #p = 0,5158
cor.test(bd$segmentados, bd$peso) #p = 0,0241
cor.test(bd$segmentados, bd$altura) #p = 0,0006

cor.test(bd$segmentados, bd$segmentados_m) #p = 9,669e
cor.test(bd$segmentados, bd$bastonados) #p = 0,0257
cor.test(bd$segmentados, bd$linfocitos) #p = 2,2e
cor.test(bd$segmentados, bd$linfocitos_m) #p = 3,263e
cor.test(bd$segmentados, bd$monocitos) #p = 0,0842
cor.test(bd$segmentados, bd$monocitos_m) #p = 0,3923
cor.test(bd$segmentados, bd$eosinofilos) #p = 0,5497
cor.test(bd$segmentados, bd$eosinofilos_m) #p = 0,2311
cor.test(bd$segmentados, bd$basofilos) #p = 0,166
cor.test(bd$segmentados, bd$basofilos_m) #p = 0,4227
cor.test(bd$segmentados, bd$plaquetas) #p = 0,3914
cor.test(bd$segmentados, bd$plaquetas_vol) #p = 0,92

#segmentado_m
cor.test(bd$segmentados_m, bd$shaps) #p=0,6017
cor.test(bd$segmentados_m, bd$acips_total) #p = 0,4437
cor.test(bd$segmentados_m, bd$teps_total) #p = 0,0774
cor.test(bd$segmentados_m, bd$hads_total) #p = 0,8098
cor.test(bd$segmentados_m, bd$neuroticismo) #p = 0,5922
cor.test(bd$segmentados_m, bd$PCR) #p = 0,8136
cor.test(bd$segmentados_m, bd$CRH) #p = 0,2484
cor.test(bd$segmentados_m, bd$cortisol) #p = 0,8027
cor.test(bd$segmentados_m, bd$idade) #p = 0,0149
cor.test(bd$segmentados_m, bd$tempo_casado) #p = 0,3782
cor.test(bd$segmentados_m, bd$num_filhos) #p = 0,9985
cor.test(bd$segmentados_m, bd$renda_familia) #p = 0,3234
cor.test(bd$segmentados_m, bd$peso) #p = 0,956
cor.test(bd$segmentados_m, bd$altura) #p = 0,8457

cor.test(bd$segmentados_m, bd$bastonados) #p = 0,0004
cor.test(bd$segmentados_m, bd$linfocitos) #p = 2,473e
cor.test(bd$segmentados_m, bd$linfocitos_m) #p = 0,4675
cor.test(bd$segmentados_m, bd$monocitos) #p = 0,0122
cor.test(bd$segmentados_m, bd$monocitos_m) #p = 4,496e
cor.test(bd$segmentados_m, bd$eosinofilos) #p = 0,5873
cor.test(bd$segmentados_m, bd$eosinofilos_m) #p = 0,0512
cor.test(bd$segmentados_m, bd$basofilos) #p = 0,105
cor.test(bd$segmentados_m, bd$basofilos_m) #p = 0,8046
cor.test(bd$segmentados_m, bd$plaquetas) #p = 0,3988
cor.test(bd$segmentados_m, bd$plaquetas_vol) #p = 0,3488

#bastonados
cor.test(bd$bastonados, bd$shaps) #p=0,5602
cor.test(bd$bastonados, bd$acips_total) #p = 0,8609
cor.test(bd$bastonados, bd$teps_total) #p = 0,9972
cor.test(bd$bastonados, bd$hads_total) #p = 0,3483
cor.test(bd$bastonados, bd$neuroticismo) #p = 0,645
cor.test(bd$bastonados, bd$PCR) #p = 0,4597
cor.test(bd$bastonados, bd$CRH) #p = 0,4861
cor.test(bd$bastonados, bd$cortisol) #p = 0,8222
cor.test(bd$bastonados, bd$idade) #p = 0,085
cor.test(bd$bastonados, bd$tempo_casado) #p = 0,9679
cor.test(bd$bastonados, bd$num_filhos) #p = 0,7416
cor.test(bd$bastonados, bd$renda_familia) #p = 0,802
cor.test(bd$bastonados, bd$peso) #p = 0,8217
cor.test(bd$bastonados, bd$altura) #p = 0,8364

cor.test(bd$bastonados, bd$linfocitos) #p = 0,0121
cor.test(bd$bastonados, bd$linfocitos_m) #p = 0,4401
cor.test(bd$bastonados, bd$monocitos) #p = 0,9446
cor.test(bd$bastonados, bd$monocitos_m) #p = 0,0164
cor.test(bd$bastonados, bd$eosinofilos) #p = 0,5597
cor.test(bd$bastonados, bd$eosinofilos_m) #p = 0,8435
cor.test(bd$bastonados, bd$basofilos) #p = 0,9382
cor.test(bd$bastonados, bd$basofilos_m) #p = 0,4489
cor.test(bd$bastonados, bd$plaquetas) #p = 0,1715
cor.test(bd$bastonados, bd$plaquetas_vol) #p = 0,0358

#linfocitos
cor.test(bd$linfocitos, bd$shaps) #p=0,0413
cor.test(bd$linfocitos, bd$acips_total) #p = 0,3698
cor.test(bd$linfocitos, bd$teps_total) #p = 0,0344
cor.test(bd$linfocitos, bd$hads_total) #p = 0,3769
cor.test(bd$linfocitos, bd$neuroticismo) #p = 0,4334
cor.test(bd$linfocitos, bd$PCR) #p = 0,6179
cor.test(bd$linfocitos, bd$CRH) #p = 0,1738
cor.test(bd$linfocitos, bd$cortisol) #p = 0,6157
cor.test(bd$linfocitos, bd$idade) #p = 0,9058
cor.test(bd$linfocitos, bd$tempo_casado) #p = 0,6468
cor.test(bd$linfocitos, bd$num_filhos) #p = 0,8338
cor.test(bd$linfocitos, bd$renda_familia) #p = 0,802
cor.test(bd$linfocitos, bd$peso) #p = 0,0369
cor.test(bd$linfocitos, bd$altura) #p = 0,0004

cor.test(bd$linfocitos, bd$linfocitos_m) #p = 4,454e
cor.test(bd$linfocitos, bd$monocitos) #p = 0,6634
cor.test(bd$linfocitos, bd$monocitos_m) #p = 0,1603
cor.test(bd$linfocitos, bd$eosinofilos) #p = 0,0664
cor.test(bd$linfocitos, bd$eosinofilos_m) #p = 0,0282
cor.test(bd$linfocitos, bd$basofilos) #p = 0,5258
cor.test(bd$linfocitos, bd$basofilos_m) #p = 0,8705
cor.test(bd$linfocitos, bd$plaquetas) #p = 0,6422
cor.test(bd$linfocitos, bd$plaquetas_vol) #p = 0,4368

#linfocitos_m
cor.test(bd$linfocitos_m, bd$shaps) #p=0,1661
cor.test(bd$linfocitos_m, bd$acips_total) #p = 0,6566
cor.test(bd$linfocitos_m, bd$teps_total) #p = 0,7991
cor.test(bd$linfocitos_m, bd$hads_total) #p = 0,4712
cor.test(bd$linfocitos_m, bd$neuroticismo) #p = 0,1873
cor.test(bd$linfocitos_m, bd$PCR) #p = 0,9515
cor.test(bd$linfocitos_m, bd$CRH) #p = 0,8723
cor.test(bd$linfocitos_m, bd$cortisol) #p = 0,982
cor.test(bd$linfocitos_m, bd$idade) #p = 0,0044
cor.test(bd$linfocitos_m, bd$tempo_casado) #p = 0,5178
cor.test(bd$linfocitos_m, bd$num_filhos) #p = 0,6974
cor.test(bd$linfocitos_m, bd$renda_familia) #p = 0,6444
cor.test(bd$linfocitos_m, bd$peso) #p = 0,015
cor.test(bd$linfocitos_m, bd$altura) #p = 3,449e

cor.test(bd$linfocitos_m, bd$monocitos) #p = 0,0428
cor.test(bd$linfocitos_m, bd$monocitos_m) #p = 0,1912
cor.test(bd$linfocitos_m, bd$eosinofilos) #p = 0,2673
cor.test(bd$linfocitos_m, bd$eosinofilos_m) #p = 0,8633
cor.test(bd$linfocitos_m, bd$basofilos) #p = 0,5889
cor.test(bd$linfocitos_m, bd$basofilos_m) #p = 0,5085
cor.test(bd$linfocitos_m, bd$plaquetas) #p = 0,8062
cor.test(bd$linfocitos_m, bd$plaquetas_vol) #p = 0,7994

#monocitos
cor.test(bd$monocitos, bd$shaps) #p=0,9756
cor.test(bd$monocitos, bd$acips_total) #p = 0,6766
cor.test(bd$monocitos, bd$teps_total) #p = 0,198
cor.test(bd$monocitos, bd$hads_total) #p = 0,1857
cor.test(bd$monocitos, bd$neuroticismo) #p = 0,7202
cor.test(bd$monocitos, bd$PCR) #p = 0,6831
cor.test(bd$monocitos, bd$CRH) #p = 0,9604
cor.test(bd$monocitos, bd$cortisol) #p = 0,1538
cor.test(bd$monocitos, bd$idade) #p = 0,069
cor.test(bd$monocitos, bd$tempo_casado) #p = 0,3787
cor.test(bd$monocitos, bd$num_filhos) #p = 0,6854
cor.test(bd$monocitos, bd$renda_familia) #p = 0,9716
cor.test(bd$monocitos, bd$peso) #p = 0,3789
cor.test(bd$monocitos, bd$altura) #p = 0,3094

cor.test(bd$monocitos, bd$monocitos_m) #p = 0,0008
cor.test(bd$monocitos, bd$eosinofilos) #p = 0,2182
cor.test(bd$monocitos, bd$eosinofilos_m) #p = 0,041
cor.test(bd$monocitos, bd$basofilos) #p = 0,6532
cor.test(bd$monocitos, bd$basofilos_m) #p = 0,1859
cor.test(bd$monocitos, bd$plaquetas) #p = 0,3599
cor.test(bd$monocitos, bd$plaquetas_vol) #p = 0,3753

#monocitos_m
cor.test(bd$monocitos_m, bd$shaps) #p=0,6904
cor.test(bd$monocitos_m, bd$acips_total) #p = 0,4932
cor.test(bd$monocitos_m, bd$teps_total) #p = 0,8514
cor.test(bd$monocitos_m, bd$hads_total) #p = 0,7164
cor.test(bd$monocitos_m, bd$neuroticismo) #p = 0,8859
cor.test(bd$monocitos_m, bd$PCR) #p = 0,7917
cor.test(bd$monocitos_m, bd$CRH) #p = 0,6866
cor.test(bd$monocitos_m, bd$cortisol) #p = 0,0731
cor.test(bd$monocitos_m, bd$idade) #p = 0,173
cor.test(bd$monocitos_m, bd$tempo_casado) #p = 0,1275
cor.test(bd$monocitos_m, bd$num_filhos) #p = 0,9783
cor.test(bd$monocitos_m, bd$renda_familia) #p = 0,2753
cor.test(bd$monocitos_m, bd$peso) #p = 0,0257
cor.test(bd$monocitos_m, bd$altura) #p = 0,8176

cor.test(bd$monocitos_m, bd$eosinofilos) #p = 0,2931
cor.test(bd$monocitos_m, bd$eosinofilos_m) #p = 0,794
cor.test(bd$monocitos_m, bd$basofilos) #p = 0,1189
cor.test(bd$monocitos_m, bd$basofilos_m) #p = 0,738
cor.test(bd$monocitos_m, bd$plaquetas) #p = 0,6642
cor.test(bd$monocitos_m, bd$plaquetas_vol) #p = 0,333

#eosinofilos
cor.test(bd$eosinofilos, bd$shaps) #p=0,2228
cor.test(bd$eosinofilos, bd$acips_total) #p = 0,3172
cor.test(bd$eosinofilos, bd$teps_total) #p = 0,0309
cor.test(bd$eosinofilos, bd$hads_total) #p = 0,3517
cor.test(bd$eosinofilos, bd$neuroticismo) #p = 0,3854
cor.test(bd$eosinofilos, bd$PCR) #p = 0,7023
cor.test(bd$eosinofilos, bd$CRH) #p = 0,4604
cor.test(bd$eosinofilos, bd$cortisol) #p = 0,0003
cor.test(bd$eosinofilos, bd$idade) #p = 0,1577
cor.test(bd$eosinofilos, bd$tempo_casado) #p = 0,0789
cor.test(bd$eosinofilos, bd$num_filhos) #p = 0,3061
cor.test(bd$eosinofilos, bd$renda_familia) #p = 0,4789
cor.test(bd$eosinofilos, bd$peso) #p = 0,734
cor.test(bd$eosinofilos, bd$altura) #p = 0,5386

cor.test(bd$eosinofilos, bd$eosinofilos_m) #p = 2,2e
cor.test(bd$eosinofilos, bd$basofilos) #p = 0,0114
cor.test(bd$eosinofilos, bd$basofilos_m) #p = 0,005
cor.test(bd$eosinofilos, bd$plaquetas) #p = 0,5349
cor.test(bd$eosinofilos, bd$plaquetas_vol) #p = 0,0703

#eosinofilos_m
cor.test(bd$eosinofilos_m, bd$shaps) #p=0,2346
cor.test(bd$eosinofilos_m, bd$acips_total) #p = 0,3303
cor.test(bd$eosinofilos_m, bd$teps_total) #p = 0,0177
cor.test(bd$eosinofilos_m, bd$hads_total) #p = 0,386
cor.test(bd$eosinofilos_m, bd$neuroticismo) #p = 0,4135
cor.test(bd$eosinofilos_m, bd$PCR) #p = 0,9782
cor.test(bd$eosinofilos_m, bd$CRH) #p = 0,3645
cor.test(bd$eosinofilos_m, bd$cortisol) #p = 0,0001
cor.test(bd$eosinofilos_m, bd$idade) #p = 0,0245
cor.test(bd$eosinofilos_m, bd$tempo_casado) #p = 0,173
cor.test(bd$eosinofilos_m, bd$num_filhos) #p = 0,435
cor.test(bd$eosinofilos_m, bd$renda_familia) #p = 0,3044
cor.test(bd$eosinofilos_m, bd$peso) #p = 0,8893
cor.test(bd$eosinofilos_m, bd$altura) #p = 0,4075

cor.test(bd$eosinofilos_m, bd$basofilos) #p = 0,1023
cor.test(bd$eosinofilos_m, bd$basofilos_m) #p = 0,0121
cor.test(bd$eosinofilos_m, bd$plaquetas) #p = 0,6249
cor.test(bd$eosinofilos_m, bd$plaquetas_vol) #p = 0,0479

#basofilos
cor.test(bd$basofilos, bd$shaps) #p=0,2653
cor.test(bd$basofilos, bd$acips_total) #p = 0,5577
cor.test(bd$basofilos, bd$teps_total) #p = 0,657
cor.test(bd$basofilos, bd$hads_total) #p = 0,6147
cor.test(bd$basofilos, bd$neuroticismo) #p = 0,9055
cor.test(bd$basofilos, bd$PCR) #p = 0,9682
cor.test(bd$basofilos, bd$CRH) #p = 0,7813
cor.test(bd$basofilos, bd$cortisol) #p = 0,2901
cor.test(bd$basofilos, bd$idade) #p = 0,6837
cor.test(bd$basofilos, bd$tempo_casado) #p = 0,1466
cor.test(bd$basofilos, bd$num_filhos) #p = 0,6107
cor.test(bd$basofilos, bd$renda_familia) #p = 0,6477
cor.test(bd$basofilos, bd$peso) #p = 0,6839
cor.test(bd$basofilos, bd$altura) #p = 0,6659

cor.test(bd$basofilos, bd$basofilos_m) #p = 2,2e
cor.test(bd$basofilos, bd$plaquetas) #p = 0,3636
cor.test(bd$basofilos, bd$plaquetas_vol) #p = 0,7183

#basofilos_m
cor.test(bd$basofilos_m, bd$shaps) #p=0,2832
cor.test(bd$basofilos_m, bd$acips_total) #p = 0,3981
cor.test(bd$basofilos_m, bd$teps_total) #p = 0,4491
cor.test(bd$basofilos_m, bd$hads_total) #p = 0,7207
cor.test(bd$basofilos_m, bd$neuroticismo) #p = 0,854
cor.test(bd$basofilos_m, bd$PCR) #p = 0,8788
cor.test(bd$basofilos_m, bd$CRH) #p = 0,5255
cor.test(bd$basofilos_m, bd$cortisol) #p = 0,1884
cor.test(bd$basofilos_m, bd$idade) #p = 0,181
cor.test(bd$basofilos_m, bd$tempo_casado) #p = 0,1839
cor.test(bd$basofilos_m, bd$num_filhos) #p = 0,7267
cor.test(bd$basofilos_m, bd$renda_familia) #p = 0,5824
cor.test(bd$basofilos_m, bd$peso) #p = 0,2638
cor.test(bd$basofilos_m, bd$altura) #p = 0,2558

cor.test(bd$basofilos_m, bd$plaquetas) #p = 0,4391
cor.test(bd$basofilos_m, bd$plaquetas_vol) #p = 0,9083

#plaquetas
cor.test(bd$plaquetas, bd$shaps) #p=0,3667
cor.test(bd$plaquetas, bd$acips_total) #p = 0,7464
cor.test(bd$plaquetas, bd$teps_total) #p = 0,9528
cor.test(bd$plaquetas, bd$hads_total) #p = 0,7525
cor.test(bd$plaquetas, bd$neuroticismo) #p = 0,594
cor.test(bd$plaquetas, bd$PCR) #p = 0,8544
cor.test(bd$plaquetas, bd$CRH) #p = 0,282
cor.test(bd$plaquetas, bd$cortisol) #p = 0,1165
cor.test(bd$plaquetas, bd$idade) #p = 0,106
cor.test(bd$plaquetas, bd$tempo_casado) #p = 0,0853
cor.test(bd$plaquetas, bd$num_filhos) #p = 0,5389
cor.test(bd$plaquetas, bd$renda_familia) #p = 0,7051
cor.test(bd$plaquetas, bd$peso) #p = 0,7541
cor.test(bd$plaquetas, bd$altura) #p = 0,4719

cor.test(bd$plaquetas, bd$plaquetas_vol) #p = 4,28e

#plaquetas volume
cor.test(bd$plaquetas_vol, bd$shaps) #p=0,2094
cor.test(bd$plaquetas_vol, bd$acips_total) #p = 0,3114
cor.test(bd$plaquetas_vol, bd$teps_total) #p = 0,25
cor.test(bd$plaquetas_vol, bd$hads_total) #p = 0,6826
cor.test(bd$plaquetas_vol, bd$neuroticismo) #p = 0,5803
cor.test(bd$plaquetas_vol, bd$PCR) #p = 0,4775
cor.test(bd$plaquetas_vol, bd$CRH) #p = 0,4866
cor.test(bd$plaquetas_vol, bd$cortisol) #p = 0,0399
cor.test(bd$plaquetas_vol, bd$idade) #p = 0,3608
cor.test(bd$plaquetas_vol, bd$tempo_casado) #p = 0,0953
cor.test(bd$plaquetas_vol, bd$num_filhos) #p = 0,3728
cor.test(bd$plaquetas_vol, bd$renda_familia) #p = 0,4762
cor.test(bd$plaquetas_vol, bd$peso) #p = 0,9738
cor.test(bd$plaquetas_vol, bd$altura) #p = 0,3823

#teste normal
shapiro.test(bd$acips_total)
shapiro.test(bd$shaps)
