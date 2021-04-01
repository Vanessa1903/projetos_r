

##############

library(tidyverse)

bd <- read.csv("~/projetos_r/novo_huu"); head(bd)
write.csv(bd, file = "~/projetos_r/novo_huu")

#####grupos#####
socioeco <- read.csv("~/projetos_r/socioeco"); head(socioeco)
socioeco$X.1 <- NULL
socioeco$X <- NULL
names(socioeco)

questio <- read.csv("~/projetos_r/questio"); head(questio)
#names(questio)

exames <- read.csv("~/projetos_r/exames"); head(exames)

quant <- read.csv("~/projetos_r/quant"); head(quant)

quali <- read.csv("~/projetos_r/quali"); head(quali)

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


###########TESTE 1 - MODELO LINEAR###########
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

##############PROPOSTA 1: TEPS VARIAVEL DEPENDENTE################3

###INDEPENDENTES: sociodemograficas e exames

##sugestoes: idade, imc, hemograma, cortisol, crh, pcr
names(bd)

m1 <- lm(teps_total ~ idade + imc + cortisol + CRH + PCR + sexo +
           hemacias + hemoglobina + hematocrito + volumeglob +
           hemoglobina_concent + rdw + leucocitos + segmentados +
           bastonados + linfocitos + monocitos + eosinofilos + 
           basofilos + plaquetas + plaquetas_vol, bd)
summary(m1)
anova(m1)
plot(m1)
na.omit(step(m1))

##mudou
m2 <- lm(teps_total ~ idade + imc + cortisol + CRH + PCR + sexo + hemacias + 
           hemoglobina + hematocrito + volumeglob + hemoglobina_concent + 
           rdw + segmentados + bastonados + linfocitos + monocitos + 
           plaquetas + plaquetas_vol, bd)
summary(m2)
na.omit(step(m2))
anova(m2) #segmentados signif
AIC(m1, m2) #m2
vif(m2)

m3 <- lm(teps_total ~ idade + imc + cortisol + CRH + PCR + sexo + hemacias + 
           hemoglobina + volumeglob + hemoglobina_concent + 
           rdw + segmentados + bastonados + linfocitos + monocitos + 
           plaquetas + plaquetas_vol, bd)
#tira hematocrito, hemacias, hemoglobina, volumeglob, hemog_concent
#deixa volumeglob
summary(m3)
anova(m3)
AIC(m2, m3) #m2

m4 <- lm(teps_total ~ idade + imc + cortisol + CRH + PCR + sexo + volumeglob
           + rdw + segmentados + bastonados + linfocitos + monocitos + 
           plaquetas + plaquetas_vol, bd)
summary(m4)
anova(m4)
AIC(m3, m4) #m4
na.omit(step(m4))

m5 <- lm(teps_total ~ idade + PCR + linfocitos + plaquetas + plaquetas_vol, bd)
summary(m5)
anova(m5)
AIC(m5, m4) #m5
na.omit(step(m5))
vif(m5)
#tira plaquetas (valor-p muito alto)

m6 <- lm(teps_total ~ idade + PCR + linfocitos + plaquetas_vol, bd)
summary(m6)
anova(m6)
AIC(m5, m6) #m5

#tentando tirar a idade
m7 <- lm(teps_total ~ PCR + linfocitos + plaquetas + plaquetas_vol, bd)
summary(m7)
anova(m7)
AIC(m5, m7) #m5

#############MELHORANDO M5#############################

mod <- data.frame(bd$teps_total, bd$idade, bd$PCR, bd$linfocitos, bd$plaquetas,
                  bd$plaquetas_vol)

m <- na.omit(mod); head(m)
round(cor(m),2)   #matriz de correlacao
###correlacao de todos baixa com o teps
##baixa correlacao entre eles
vif(mod)
##sem multicolinearidade
m5 <- lm(teps_total ~ idade + PCR + linfocitos + plaquetas + plaquetas_vol, bd)
summary(m5)
anova(m5)

####teste de residuos

#normalidade
shapiro.test(rstudent(m5))    #-aceita p>0,05   #residuos
qqPlot(rstudent(m5), pch=16)  #dentro do envelope   #residuos
#variancia - homoscedasticidade
plot(predict(m5), rstudent(m5), pch=19, ylim=c(-3,3)); abline(h=c(-3,0,3),lty=2)
bartlett.test(mod) 
#na.omit(bartlett.test(teps_total ~ idade + PCR + linfocitos + plaquetas + plaquetas_vol, bd))
#nao funcionou







