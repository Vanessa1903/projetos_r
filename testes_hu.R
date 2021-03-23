

###########TESTES DE CORRELA??O###################

##################teste cor##############
###valor-p < 0,05 - Rejeita Ho
###valor-p > 0,05 - Aceita Ho
##Ho: nao h? correlacao
##H1: h? correlacao



###!!!correlacao teste e do pairs ta dando diferente

##shaps
cor.test(bd$shaps, bd$acips_total) #p = 1,302e < 0,05
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







#teste normal
shapiro.test(bd$acips_total)
shapiro.test(bd$shaps)
