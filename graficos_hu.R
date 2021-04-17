

##############

library(tidyverse)

bd <- read.csv("~/projetos_r/novo_huu"); head(bd)
bd$X <- NULL
#write.csv(bd, file = "~/projetos_r/novo_huu")
names(bd)

#####grupos#####
library(tidyverse)
socioeco <- read.csv("~/projetos_r/socioeco"); head(socioeco)
socioeco$X <- NULL
names(socioeco)

questio <- read.csv("~/projetos_r/questio"); head(questio)
questio$X <- NULL
names(questio)

exames <- read.csv("~/projetos_r/exames"); head(exames)
exames$X <- NULL
names(exames)

quant <- read.csv("~/projetos_r/quant"); head(quant)
quant$X <- NULL
names(quant)

quali <- read.csv("~/projetos_r/quali"); head(quali)
quali$X <- NULL
names(quali)

dim(bd) #ver dimensao do bd
par(bd)

##Grafico 3 variaveis ou mais

#A expressao divide as linhas das colunas atraves de ~, i.e., antes do sinal ~ 
#especificaremos as linhas e depois dele, as colunas. os sinais + e * indicam 
#se desejamos apenas adicionar as variaveis ou cruza-las, respectivamente.

#pairs(~ shaps + acips_total + idade, bd, na.action = na.omit, upper.panel = NULL, 
      #col = as.factor(bd$sexo))

###adicionando linha de tendencia lower.panel = panel.smooth
#pairs(~ shaps + acips_total + idade, bd, na.action = na.omit, upper.panel = NULL, 
 #     col = as.factor(bd$sexo), lower.panel = panel.smooth)

###ouu adicionando linha de regressao
panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                      cex = 1, col.line="red") {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) {
    abline(lm(y[ok]~x[ok]), col = col.line)
  }
}
pairs(~ shaps + acips_total + idade, bd, na.action = na.omit, upper.panel = NULL, 
      col = as.factor(bd$sexo), lower.panel = panel.lm)

##adicionando a correlacao no grafico

panel.cor <- function(x, y, digits = 3, prefix = "", ...) { #cex.cor
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  #if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 2) #, cex = cex.cor * r  
}
#correlacao vai aparecer todas positivas

library(tidyverse)
#bd <- mutate(bd, PCR, CRH, cortisol)
#bd$Amostra <- NULL
names(bd)

##os comentados fazer o tamanho da letra ser proporcional ao numero

###Dispers?o - question?rios x exames
pairs(~ shaps + acips_total + teps_total + hads_total + neuroticismo 
      + PCR + CRH + cortisol, bd, na.action = na.omit, upper.panel = panel.cor,
      pch = 19, lwd = 2, col = as.factor(bd$sexo), lower.panel = panel.smooth)

##Dispers?o - question?rios x socioeconomico
pairs(~ shaps + acips_total + teps_total + hads_total + neuroticismo 
      + idade + tempo_casado + num_filhos + renda_familia
      + peso + altura, bd, na.action = na.omit, upper.panel = panel.cor,
      pch = 19, lwd = 2, col = as.factor(bd$sexo), lower.panel = panel.smooth)


###########TESTE 1 - MODELO LINEAR#####################
#####STEPWISE#####
#X = shaps, acips, hads, segmentados, linfocitos, eosinofilos

modelo1 <- lm(teps_total ~ shaps + acips_total + hads_total + segmentados +
          linfocitos + eosinofilos, bd)
plot(modelo1)
summary(modelo1)
step(modelo1, direction = "both", scale = 12.65)
###mandou usar todas as variaveis
anova(modelo1)

modelo2 <- lm(teps_total ~ shaps + acips_total + eosinofilos, bd)
modelo2
anova(modelo2)
summary(modelo2)

modelo3 <- lm(teps_total ~ shaps + acips_total + eosinofilos + leucocitos +
                segmentados, bd)
step(modelo3, na.action = na.omit)
modelo3
?step
summary(modelo3)
names(bd)

modelo4 <- lm(teps_total ~ shaps + acips_total + eosinofilos +
                segmentados, bd)
modelo4
summary(modelo4)
names(bd)

modelo5 <- lm(teps_total ~ acips_total + eosinofilos + segmentados, bd)
summary(modelo5)


AIC(modelo2, modelo5)
#ficaria com o 5
AIC(modelo4, modelo5)

anova(modelo1, modelo2)#teste de razao da verossimilhanca (só p/ modelo encaixado)
#H0: modelo simples é melhor (- param)
#H1: modelo mais complexo é melhor (+ param)
#valor-p > 5%, aceita H0
#neste caso, aceita modelo 2

##senao fosse encaixado: bic ou aic
AIC(modelo1, modelo2)
#df-numero de param
#aic: geralmente bate

#ultimo parametro referente ao erro (variancia dele - soma do quad do residuo (SQRes))


#analise de residuos: verificar pressupostos

####Graf: residuos estimados:
#Podemos utilizar este gráfico para observar a independência e a 
#homocedasticidade, se os resíduos se distribuem de maneira 
#razoavelmente aleatória e com mesma amplitude em torno do zero.

####Graf: normal Q-Q
#podemos avaliar a normalidade dos resíduos. A linha diagonal pontilhada 
#representa a distribuição normal teórica, e os pontos a distribuição 
#dos resíduos observada. Espera-se que não exista grande fuga dos pontos 
#em relação à reta teórica.

###Graf: scale-location
#O terceiro gráfico pode ser avaliado da mesma maneira que o primeiro, 
#observando a aleatoriedade e amplitude, desta vez dos resíduos 
#padronizados.

###Graf: distancias de Cook
#E o último gráfico permite visualizar as Distâncias de Cook
#das observações, uma medida de influência que pode indicar a presença 
#de outliers quando possui valor maior do que 1.

shapiro.test(modelo2$residuals) #normalidade erro
#O p-valor se refere à hipótese de que os resíduos seguem de fato 
#uma distribuição Normal, e essa hipótese é rejeitada, de modo geral, 
#quando p é menor que 0.05

require(car)
require(MASS)
#--- Normalidade de resíduos
# 1) Teste de Shapiro-Wilk
shapiro.test(rstudent(modelo2)) 
#residuo do modelo

# 2.1) Teste de Bartlett (Só se tiver normalidade!)
#bartlett.test(teps_total ~ shaps, data = bd) 


# 2.2) Teste de Levene #homogeneidade de variancias (quando rejeita normalidade)
#leveneTest(teps_total ~ shaps + acips_total + eosinofilos, data=bd)
#par(mfrow=c(1,2))

# 3) Gráfico com envelope
qqPlot(rstudent(modelo2), pch=16)
#tem q estar dentro do envelope
#fora é outlier

# 4) Gráfico de preditos X resíduos studentizados
plot(predict(modelo2), rstudent(modelo2), pch=19, ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2)
#identify(predict(mA), rstudent(mA))
#limite de -3,3, fora disso é outlier
par(mfrow=c(1,1))

##############PROPOSTA 1: TEPS VARIAVEL DEPENDENTE#######################

###INDEPENDENTES: sociodemograficas e exames

##sugestoes: idade, imc, hemograma, cortisol, crh, pcr
names(bd)

m1 <- na.omit(lm(teps_total ~ idade + cortisol + CRH + PCR +
           hemoglobina_concent + rdw + leucocitos + segmentados +
           bastonados + linfocitos + monocitos + eosinofilos + 
           basofilos + plaquetas_vol + anos_estudo + renda_familia +
           tempo_tabagismo + exercicio_tempo +
           exercicio_duracao + traumatismo_tempo + internacao_quant, bd))
summary(m1)
anova(m1)
plot(m1)
na.omit(step(m1))

names(quant)

##mudou
m2 <- lm(teps_total ~ idade + CRH + PCR + hemoglobina_concent + 
           leucocitos + segmentados + linfocitos + monocitos + eosinofilos + 
           plaquetas_vol + anos_estudo + renda_familia + exercicio_tempo + 
           exercicio_duracao + internacao_quant, bd)
bd$exercicio_duracao
bd$exercicio_tempo
bd$internacao_quant
summary(m2) #R: 0,6854
anova(m2) 
##menos contribuem - valor-p
#cor - renda (-0,0855), leucocitos (-0,1912), idade (-0,2164)
AIC(m1, m2) #m2
vif(m2)
step(m2)
##VIF>=10 perigoso: cor - segmentados (-0,2948), linfocitos (0,3159), monocitos (0,1955)
#eosinofilos (-0,322)
m_perigo <- lm(teps_total ~ segmentados + linfocitos + monocitos + eosinofilos, bd)
summary(m_perigo)
vif(m_perigo)
step(m_perigo) #segmentados e eosinofilos

##ACP: menos contribuem
#PC19 = renda, idade, leucocitos (qualquer um q eu tire, o m2 continua sendo preferivel)

####teste de residuos
#normalidade
shapiro.test(rstudent(m2))    #-aceita p>0,05   #residuos
qqPlot(rstudent(m2), pch=16)  #dentro do envelope   #residuos
#variancia - homoscedasticidade
plot(predict(m2), rstudent(m2), pch=19, ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2)

##sem linfocito e monocito
m2_2 <- lm(teps_total ~ idade + CRH + PCR + hemoglobina_concent + 
             leucocitos + segmentados + eosinofilos +
             plaquetas_vol + anos_estudo + renda_familia + exercicio_tempo + 
             exercicio_duracao + internacao_quant, bd)
summary(m2_2) #R: 0,1541
AIC(m2, m2_2) #m2, mas tem multic
vif(m2_2)
step(m2_2)
####teste de residuos
#normalidade
shapiro.test(rstudent(m2_2))    #-aceita p>0,05   #residuos
qqPlot(rstudent(m2_2), pch=16)  #dentro do envelope   #residuos
#variancia - homoscedasticidade
plot(predict(m2_2), rstudent(m2_2), pch=19, ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2)


m3 <- lm(teps_total ~ idade + CRH + PCR + hemoglobina_concent + leucocitos + 
           segmentados + eosinofilos + plaquetas_vol + anos_estudo + 
           renda_familia + exercicio_tempo, bd)
summary(m3) #0,2945
vif(m3)
AIC(m2, m3) #m2, mas o m3 nao tem multicolinearidade
AIC(m3, m2_2) #m3
step(m3)
####teste de residuos
#normalidade
shapiro.test(rstudent(m3))    #-aceita p>0,05   #residuos
qqPlot(rstudent(m3), pch=16)  #dois fora do envelope   #residuos
#variancia - homoscedasticidade
plot(predict(m3), rstudent(m3), pch=19, ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2)
    #um fora

##tirando idade
m3_3 <- lm(teps_total ~ CRH + PCR + hemoglobina_concent + leucocitos + 
           segmentados + eosinofilos + plaquetas_vol + anos_estudo + 
           renda_familia + exercicio_tempo, bd)
summary(m3_3) #0,3469
vif(m3_3)
AIC(m3, m3_3, m2) #m2, mas o m3 nao tem multicolinearidade 
##ou m_3_3
step(m3_3)
####teste de residuos
#normalidade
shapiro.test(rstudent(m3_3))    #-aceita p>0,05   #residuos
qqPlot(rstudent(m3_3), pch=16)  #dois fora do envelope   #residuos
#variancia - homoscedasticidade
plot(predict(m3_3), rstudent(m3_3), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ok









####modelo 2 - tentativas de melhorar

m2 <- lm(teps_total ~ idade + CRH + PCR + hemoglobina_concent + 
           leucocitos + segmentados + linfocitos + monocitos + eosinofilos + 
           plaquetas_vol + anos_estudo + renda_familia + exercicio_tempo + 
           exercicio_duracao + internacao_quant, bd)
summary(m2) #R: 0,6854
anova(m2) 
##menos contribuem - valor-p
#cor - renda (-0,0855), leucocitos (-0,1912), idade (-0,2164)

AIC(m1, m2) #m2
vif(m2)
step(m2)
##VIF>=10 perigoso: cor - segmentados (-0,2948), linfocitos (0,3159), monocitos (0,1955)
#eosinofilos (-0,322)

##ACP: menos contribuem
#PC19 = renda, idade, leucocitos (qualquer um q eu tire, o m2 continua sendo preferivel)

####teste de residuos
#normalidade
shapiro.test(rstudent(m2))    #-aceita p>0,05   #residuos
qqPlot(rstudent(m2), pch=16)  #dentro do envelope   #residuos
#variancia - homoscedasticidade
plot(predict(m2), rstudent(m2), pch=19, ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2)

m_idade <- lm(teps_total ~ idade, bd)
summary(m_idade) #R: 0,02 p = 0,153
shapiro.test(rstudent(m_idade))    #-aceita p>0,05
qqPlot(rstudent(m_idade), pch=16)  #dentro do envelope 
plot(predict(m_idade), rstudent(m_idade), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ok

m_crh <- lm(teps_total ~ CRH, bd)
summary(m_crh) #R: -0,02 p = 0,888
shapiro.test(rstudent(m_crh))    #-aceita p>0,05
qqPlot(rstudent(m_crh), pch=16)  #dentro do envelope 
plot(predict(m_crh), rstudent(m_crh), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #com tendencia

m_pcr <- lm(teps_total ~ PCR, bd)
summary(m_pcr) #R: 0,0002 p = 0,3237
shapiro.test(rstudent(m_pcr))    #-aceita p>0,05
qqPlot(rstudent(m_pcr), pch=16)  #dentro do envelope 
plot(predict(m_pcr), rstudent(m_pcr), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #com tendencia

m_hemo <- lm(teps_total ~ hemoglobina_concent, bd)
summary(m_hemo) #R: -0,0237 p = 0,948
shapiro.test(rstudent(m_hemo))    #-aceita p>0,05
qqPlot(rstudent(m_hemo), pch=16)  #dentro do envelope 
plot(predict(m_hemo), rstudent(m_hemo), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ok

m_leuc <- lm(teps_total ~ leucocitos, bd)
summary(m_leuc) #R: 0,0141 p: 0,208
shapiro.test(rstudent(m_leuc))    #-aceita p>0,05
qqPlot(rstudent(m_leuc), pch=16)  #dentro do envelope 
plot(predict(m_leuc), rstudent(m_leuc), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #tendencia

m_seg <- lm(teps_total ~ segmentados, bd)
summary(m_seg) #R: 0,0656 p: 0,0493
shapiro.test(rstudent(m_seg))    #-aceita p>0,05
qqPlot(rstudent(m_seg), pch=16)  #dentro do envelope 
plot(predict(m_seg), rstudent(m_seg), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ok?

m_linf <- lm(teps_total ~ linfocitos, bd)
summary(m_linf) #R: 0,0789 p: 0,0345
shapiro.test(rstudent(m_linf))    #-aceita p>0,05
qqPlot(rstudent(m_linf), pch=16)  #dentro do envelope 
plot(predict(m_linf), rstudent(m_linf), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ok?

m_monoc <- lm(teps_total ~ monocitos, bd)
summary(m_monoc) #R: 0,0158 p: 0,198
shapiro.test(rstudent(m_monoc))    #-aceita p>0,05
qqPlot(rstudent(m_monoc), pch=16)  #dentro do envelope 
plot(predict(m_monoc), rstudent(m_monoc), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ok?

m_eosin <- lm(teps_total ~ eosinofilos, bd)
summary(m_eosin) #R: 0,0828 p: 0,031
shapiro.test(rstudent(m_eosin))    #-aceita p>0,05
qqPlot(rstudent(m_eosin), pch=16)  #dentro do envelope 
plot(predict(m_eosin), rstudent(m_eosin), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ok?

m_plaq <- lm(teps_total ~ plaquetas_vol, bd)
summary(m_plaq) #R: 0,0081 p: 0,250
shapiro.test(rstudent(m_plaq))    #-aceita p>0,05
qqPlot(rstudent(m_plaq), pch=16)  #dentro do envelope 
plot(predict(m_plaq), rstudent(m_plaq), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ok

m_anose <- lm(teps_total ~ anos_estudo, bd)
summary(m_anose) #R: -0,0062 p: 0,381
shapiro.test(rstudent(m_anose))    #-aceita p>0,05
qqPlot(rstudent(m_anose), pch=16)  #dentro do envelope 
plot(predict(m_anose), rstudent(m_anose), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ok

m_renda <- lm(teps_total ~ renda_familia, bd)
summary(m_renda) #R: -0,0181 p: 0,595
shapiro.test(rstudent(m_renda))    #-aceita p>0,05
qqPlot(rstudent(m_renda), pch=16)  #dentro do envelope 
plot(predict(m_renda), rstudent(m_renda), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ok?

m_exerct <- lm(teps_total ~ exercicio_tempo, bd)
summary(m_exerct) #R: 0,0549 p: 0,0734
shapiro.test(rstudent(m_exerct))    #-aceita p>0,05
qqPlot(rstudent(m_exerct), pch=16)  #dentro do envelope 
plot(predict(m_exerct), rstudent(m_exerct), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #super tendencia

m_exercd <- lm(teps_total ~ exercicio_duracao, bd)
summary(m_exercd) #R: -0,0226 p: 0,945
shapiro.test(rstudent(m_exercd))    #-aceita p>0,05
qqPlot(rstudent(m_exercd), pch=16)  #dois pontos saem do envelope
plot(predict(m_exercd), rstudent(m_exercd), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #super tendencia

m_intern <- lm(teps_total ~ internacao_quant, bd)
summary(m_intern) #R: -0,0222 p: 0,887
shapiro.test(rstudent(m_intern))    #-aceita p>0,05
qqPlot(rstudent(m_intern), pch=16)  #dois pontos saem do envelope
plot(predict(m_intern), rstudent(m_intern), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #super tendencia









##############PROPOSTA 2: TEPS VARIAVEL DEPENDENTE#######################

###INDEPENDENTES: outros questionarios


prop1 <- lm(teps_total ~ bas_total + bis_total + panas_neg + panas_pos +
              hads_total + hads_ans + hads_dep + neuroticismo +
              quesi_abuso_emoc + quesi_abuso_fis + quesi_abuso_sex +
              quesi_neglig_emoc + quesi_neglig_fis + quesi_total, bd)
summary(prop1)#42,56
na.omit(step(prop1))
vif(prop1)
##multic: hads_total, hads_ans, quesi_total
step(prop1)

problema1 <- lm(teps_total ~ hads_total + hads_ans + quesi_total, bd)
summary(problema1)
vif(problema1)

##1 - sem o hads_dep e quesi_total
prop1.1 <- lm(teps_total ~ bas_total + bis_total + panas_neg + panas_pos + 
                hads_total + hads_ans + neuroticismo + quesi_abuso_emoc + 
                quesi_abuso_fis + quesi_abuso_sex + quesi_neglig_emoc + quesi_neglig_fis, bd)
summary(prop1.1)#0,4256
na.omit(step(prop1.1))
vif(prop1.1)##hads_total e hads_ans

##tirando hads_total - multicolinearidade
prop1.2 <- lm(teps_total ~ bas_total + bis_total + panas_neg + panas_pos + 
                hads_ans + neuroticismo + quesi_abuso_emoc + 
                quesi_abuso_fis + quesi_abuso_sex + quesi_neglig_emoc + quesi_neglig_fis, bd)
summary(prop1.2)#0,278
vif(prop1.2)

##OU tirando hads_ans
prop1.3 <- lm(teps_total ~ bas_total + bis_total + panas_neg + panas_pos + 
                hads_total + neuroticismo + quesi_abuso_emoc + 
                quesi_abuso_fis + quesi_abuso_sex + quesi_neglig_emoc + quesi_neglig_fis, bd)
summary(prop1.3)#0,3916
vif(prop1.3)
AIC(prop1.2, prop1.3) #####1.3
#teste de residuos
shapiro.test(rstudent(prop1.3))    #-aceita p>0,05
qqPlot(rstudent(prop1.3), pch=16)  #dentro do envelope
plot(predict(prop1.3), rstudent(prop1.3), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ok
step(prop1.3)

prop2 <- lm(teps_total ~ bas_total + bis_total + panas_neg + hads_total + 
              neuroticismo + quesi_abuso_emoc + quesi_abuso_fis + quesi_abuso_sex + 
              quesi_neglig_fis, bd)
summary(prop2)#0,4232
vif(prop2)
AIC(prop2, prop1.3) #####2
#teste de residuos
shapiro.test(rstudent(prop2))    #-aceita p>0,05
qqPlot(rstudent(prop2), pch=16)  #dentro do envelope
plot(predict(prop2), rstudent(prop2), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ok
step(prop2)

##tirando abuso_sex
prop2.2 <- lm(teps_total ~ bas_total + bis_total + panas_neg + hads_total + 
              neuroticismo + quesi_abuso_emoc + quesi_abuso_fis + 
              quesi_neglig_fis, bd)
summary(prop2.2)#0,432
vif(prop2.2)
AIC(prop2, prop2.2) #####2.2
#teste de residuos
shapiro.test(rstudent(prop2.2))    #-aceita p>0,05
qqPlot(rstudent(prop2.2), pch=16)  #dentro do envelope
plot(predict(prop2.2), rstudent(prop2.2), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ponto na linha
step(prop2.2)

prop3 <- lm(teps_total ~ bas_total + hads_total + neuroticismo + quesi_abuso_emoc + 
              quesi_abuso_fis + quesi_neglig_fis, bd)
summary(prop3)#0,4505
vif(prop3)
AIC(prop3, prop2.2) #####3
#teste de residuos
shapiro.test(rstudent(prop3))    #-aceita p>0,05
qqPlot(rstudent(prop3), pch=16)  #ponto fora do envelope
plot(predict(prop3), rstudent(prop3), pch=19, 
     ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2) #ponto fora

















































