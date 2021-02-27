

##############GRÁFICOS DE DISPERSÃO

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

library(tidyverse)

bd <- read.csv("~/projetos_r/novo_hu"); head(bd)
#exames_novos <- read.csv("~/projetos_r/exames_novos_hu"); head(exames_novos)
#####grupos#####
socioeco <- read.csv("~/projetos_r/socioeco"); head(socioeco)
exames <- read.csv("~/projetos_r/exames"); head(exames)
questio <- read.csv("~/projetos_r/questio"); head(questio)


dim(bd) #ver dimensao do bd
par(bd)

###########TESTE 1: SHAPS X ACIPS

##supondo: y = acips, x = shaps

##correlacao
cor(bd$shaps, bd$acips_total, use = "all.obs")
#esta dando NA o resultado...
teste <- na.omit(cbind(bd$shaps, bd$acips_total)) 
cor(teste)
dim(teste)
?cor
regressao <- lm(bd$acips_total ~ bd$shaps); regressao
summary(regressao)
##como ignorar os NA??
#na.action = na.omit, na.rm = TRUE,  use = "all.obs"  ja tentei esses..



##quais variaveis escolher???
###regressao multipla selecao de variaveis
##hemograma x questionarios
##fazer grupos: socioeconomico, binarias, questionarios

bd


##graf1  ---- 2 variaveis

bd %>% ggplot(aes(shaps, acips_total)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, na.rm = FALSE) 
# Acrescenta a linha de tendência e o intervalo de confiança de predição
?formula
?geom_smooth
#geom_smooth(method=lm, se=FALSE) # Acrescenta a linha de tendência, sem o intervalo de predição

###grafico sem ggplot

plot.new()
plot(bd$acips_total ~ bd$shaps)
abline(regressao)


##graf2 -- 3 variaveis ou mais

#A expressao divide as linhas das colunas atraves de ~, i.e., antes do sinal ~ 
#especificaremos as linhas e depois dele, as colunas. os sinais + e * indicam 
#se desejamos apenas adicionar as variaveis ou cruza-las, respectivamente.

pairs(~ shaps + acips_total + idade, bd, na.action = na.omit, upper.panel = NULL, 
      col = as.factor(bd$sexo))
?pairs
?pch

##adicionando linha de tendencia lower.panel = panel.smooth
pairs(~ shaps + acips_total + idade, bd, na.action = na.omit, upper.panel = NULL, 
      col = as.factor(bd$sexo), lower.panel = panel.smooth)

##ouu adicionando linha de regressao
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

panel.cor <- function(x, y, digits = 2, prefix = "", ...) { #cex.cor
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  #if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 2) #, cex = cex.cor * r  
}
#correlacao vai aparecer todas positivas

##os comentados fazer o tamanho da letra ser proporcional ao numero
pairs(~ shaps + acips_total + idade + teps_total, bd, na.action = na.omit, upper.panel = panel.cor,
      pch = 19, lwd = 2, col = as.factor(bd$sexo), lower.panel = panel.smooth)
?text
