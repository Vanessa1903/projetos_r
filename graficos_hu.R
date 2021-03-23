

##############GRAFICOS DE DISPERSAO

#grupo_socioeco <- c(bd[,1:7], bd[,29:63]); 
#head(grupo_socioeco)
#names(grupo_socioeco)
#grupo_socioeco
#write.csv(grupo_socioeco, file = "socioeco")

#exames[51,] <- NA
#tail(exames)
#grupo_exames <- c(bd[,8:28], exames);
#head(grupo_exames)
#names(grupo_exames)
#write.csv(grupo_exames, file = "exames")

#grupo_questio <- bd[,64:84]
#grupo_questio
#head(grupo_questio)
#names(grupo_questio)
#write.csv(grupo_questio, file = "questio")

#PCR <- c(NA, 0.879, 3.757, NA, NA, 2.445, 7.795, 0.052, 12.38756121, 1.541,
 #        2.524, NA, 0.822, 0.395, 3.205, 0.871, 1.369, NA, 1.387, 0.85, 0.587,
  #       3.181, 0.118, NA, 5.176, 1.159, 3.123, NA, 2.426, 1.349, 0.461, 2.945,
   #      0.408, 2.707, NA, 0.603, NA, NA, 0.456, NA, NA, 0.885, 4.841, NA,
    #     1.767, NA, 1.538, 3.455, 1.302, 3.414, NA)
#CRH <- c(NA, 89.561, 94.723, 105.059, NA, 95.692, 104.484, 99.69, 97.152, 102.334,
#         106.045, 105.616, 106.019, 103.01, 102.382, 105.636, 105.624, 106.054,
 #        104.71, 91.182, 88.27, 105.419, 105.631, 105.508, 104.447, 105.471, 106.01,
  #       101.448, 100.861, 98.35, 98.449, 102.299, 104.537, 92.001, 65.912, 105.243,
   #      105.722, 105.399, 106.008, 93.646, 106.002, 100.182, 61.272, 99.301, 101.823,
    #     104.137, 100.548, 106.071, 103.204, 42.415, NA)
#cortisol <- c(NA, 92.68, 89.3, 191.8, NA, 77.09, 2.17, 91, 60.51, 75.16, 71.18,
 #             92.09, 87.55, 88.86, 82.29, 75.46, 89.06, 86.64, 93.23, 86.23, 82.67,
  #            30.17, 78.22, 25.28, 76.53, 65.84, 54.97, 54.82, 46.5, 74.71, 67.44,
   #           65.75, 73.14, 53.13, 89.41, 85, 63.53, 71.41, 52.04, 59.81, 23.57, 31.81,
    #          75.19, 83.11, 17.04, 68.94, 35.41, 29.06, 46.27, 30.08, NA)

library(tidyverse)

bd <- read.csv("~/projetos_r/novo_huu"); head(bd)
#write.csv(bd, file = "~/projetos_r/novo_huu")
#?write_csv
#exames_novos <- read.csv("~/projetos_r/exames_novos_hu"); head(exames_novos)
#####grupos#####
socioeco <- read.csv("~/projetos_r/socioeco"); head(socioeco)
names(socioeco)
exames <- read.csv("~/projetos_r/exames"); head(exames)
names(exames)
questio <- read.csv("~/projetos_r/questio"); head(questio)
names(questio)

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


######gr?fico com cor na correlacao

#library(corrplot)
#Mcor<- cor(bd$shaps, bd$teps_total)
#corrplot(Mcor, type = "upper", method='ellipse', tl.pos="t1")
#corrplot(Mcor, type = "lower", method='number',  tl.pos="n", cl.pos="n", cex=0.3, add=T)
#corrRect(clus=c(8,6))
?corrplot











