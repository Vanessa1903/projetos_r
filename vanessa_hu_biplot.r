###---------------------------------------------###
###          Aldir Biplot                        ###
###---------------------------------------------###

quant <- read.csv("~/projetos_r/quant"); head(quant)
names(quant)
quant$X <- NULL

questio <- read.csv("~/projetos_r/questio"); head(questio)
names(questio)
questio$X <- NULL

exames <- read.csv("~/projetos_r/exames"); head(exames)
names(exames)
exames$X <- NULL


###--- Todas as variaveis quanti
w <- na.omit(quant); head(w)
dim(w)
round(sqrt(diag(cov(w))),4) # variancia
round(cor(w),2)             # matriz de correlacao

###--- QUESTIONARIOS
v <- na.omit(questio); head(v)
dim(v)
round(sqrt(diag(cov(v))),4) # variancia
round(cor(v),2)             # matriz de correlacao


###--- EXAMES
x <- na.omit(exames); head(x)
dim(x)
round(sqrt(diag(cov(x))),4) # variancia
round(cor(x),2)             # matriz de correlacao


# grafico de correlacao
library(corrplot)

#TODOS AS QUANTI
Mcor<- cor(w)
corrplot(Mcor, type="upper", method='ellipse', tl.pos="t1", tl.cex = 0.6)
?corrplot
corrplot(Mcor, type="lower", method='number' , tl.pos="n", tl.cex = 0.6, cl.pos="n", number.cex=0.5, add=T)
# corrRect(clus=c(8,6))

#QUESTIONARIOS
Vcor<- cor(v)
corrplot(Vcor, type="upper", method='ellipse', tl.pos="t1")
corrplot(Vcor, type="lower", method='number' , tl.pos="n", cl.pos="n", number.cex=0.5, add=T)

#EXAMES
Xcor<- cor(x)
corrplot(Xcor, type="upper", method='ellipse', tl.pos="t1")
corrplot(Xcor, type="lower", method='number' , tl.pos="n", cl.pos="n", number.cex=0.5, add=T)



# Teste de correlacoes
valor_p1= c(
  cor.test(w[,1],w[, 2])[3],
  cor.test(w[,1],w[, 3])[3], 
  cor.test(w[,1],w[, 4])[3], 
  cor.test(w[,1],w[, 5])[3],
  cor.test(w[,1],w[, 6])[3],
  cor.test(w[,1],w[, 7])[3],
  cor.test(w[,1],w[, 8])[3],
  cor.test(w[,1],w[, 9])[3],
  cor.test(w[,1],w[,10])[3],
  cor.test(w[,1],w[,11])[3],
  cor.test(w[,1],w[,12])[3],
  cor.test(w[,1],w[,13])[3],
  cor.test(w[,1],w[,14])[3]); valor_p1

valor_p2= c(
  cor.test(w[,2],w[, 3])[3], 
  cor.test(w[,2],w[, 4])[3], 
  cor.test(w[,2],w[, 5])[3],
  cor.test(w[,2],w[, 6])[3],
  cor.test(w[,2],w[, 7])[3],
  cor.test(w[,2],w[, 8])[3],
  cor.test(w[,2],w[, 9])[3],
  cor.test(w[,2],w[,10])[3],
  cor.test(w[,2],w[,11])[3],
  cor.test(w[,2],w[,12])[3],
  cor.test(w[,2],w[,13])[3],
  cor.test(w[,2],w[,14])[3]); valor_p2

valor_p3= c(
  cor.test(w[,3],w[, 4])[3], 
  cor.test(w[,3],w[, 5])[3],
  cor.test(w[,3],w[, 6])[3],
  cor.test(w[,3],w[, 7])[3],
  cor.test(w[,3],w[, 8])[3],
  cor.test(w[,3],w[, 9])[3],
  cor.test(w[,3],w[,10])[3],
  cor.test(w[,3],w[,11])[3],
  cor.test(w[,3],w[,12])[3],
  cor.test(w[,3],w[,13])[3],
  cor.test(w[,3],w[,14])[3]); valor_p3

valor_p4= c(
  cor.test(w[,4],w[, 5])[3],
  cor.test(w[,4],w[, 6])[3],
  cor.test(w[,4],w[, 7])[3],
  cor.test(w[,4],w[, 8])[3],
  cor.test(w[,4],w[, 9])[3],
  cor.test(w[,4],w[,10])[3],
  cor.test(w[,4],w[,11])[3],
  cor.test(w[,4],w[,12])[3],
  cor.test(w[,4],w[,13])[3],
  cor.test(w[,4],w[,14])[3]); valor_p4

valor_p5= c(
  cor.test(w[,5],w[, 6])[3],
  cor.test(w[,5],w[, 7])[3],
  cor.test(w[,5],w[, 8])[3],
  cor.test(w[,5],w[, 9])[3],
  cor.test(w[,5],w[,10])[3],
  cor.test(w[,5],w[,11])[3],
  cor.test(w[,5],w[,12])[3],
  cor.test(w[,5],w[,13])[3],
  cor.test(w[,5],w[,14])[3]); valor_p5

valor_p6= c(
  cor.test(w[,6],w[, 7])[3],
  cor.test(w[,6],w[, 8])[3],
  cor.test(w[,6],w[, 9])[3],
  cor.test(w[,6],w[,10])[3],
  cor.test(w[,6],w[,11])[3],
  cor.test(w[,6],w[,12])[3],
  cor.test(w[,6],w[,13])[3],
  cor.test(w[,6],w[,14])[3]); valor_p6

valor_p7= c(
  cor.test(w[,7],w[, 8])[3],
  cor.test(w[,7],w[, 9])[3],
  cor.test(w[,7],w[,10])[3],
  cor.test(w[,7],w[,11])[3],
  cor.test(w[,7],w[,12])[3],
  cor.test(w[,7],w[,13])[3],
  cor.test(w[,7],w[,14])[3]); valor_p7

valor_p8= c(
  cor.test(w[,8],w[, 9])[3],
  cor.test(w[,8],w[,10])[3],
  cor.test(w[,8],w[,11])[3],
  cor.test(w[,8],w[,12])[3],
  cor.test(w[,8],w[,13])[3],
  cor.test(w[,8],w[,14])[3]); valor_p8

valor_p9= c(
  cor.test(w[,9],w[,10])[3],
  cor.test(w[,9],w[,11])[3],
  cor.test(w[,9],w[,12])[3],
  cor.test(w[,9],w[,13])[3],
  cor.test(w[,9],w[,14])[3]); valor_p9

valor_p10= c(
  cor.test(w[,10],w[,11])[3],
  cor.test(w[,10],w[,12])[3],
  cor.test(w[,10],w[,13])[3],
  cor.test(w[,10],w[,14])[3]); valor_p10

valor_p11= c(
  cor.test(w[,11],w[,12])[3],
  cor.test(w[,11],w[,13])[3],
  cor.test(w[,11],w[,14])[3]); valor_p11

valor_p12= c(
  cor.test(w[,12],w[,13])[3],
  cor.test(w[,12],w[,14])[3]); valor_p12

valor_p13= c(
  cor.test(w[,13],w[,14])[3]); valor_p13


#----- ACP  TODOS AS QUANT
acp<- prcomp(w)
summary(acp)                   # Contribuicao de cada componente principal
round(summary(acp)$rotation,2) # Contribuicao de cada variavel na CP

#----- ACP  QUESTIONARIOS
acp_v<- prcomp(v)
summary(acp_v)                   # Contribuicao de cada componente principal
round(summary(acp_v)$rotation,2) # Contribuicao de cada variavel na CP

#----- ACP  EXAMES
acp_x<- prcomp(x)
summary(acp_x)                   # Contribuicao de cada componente principal
round(summary(acp_x)$rotation,2) # Contribuicao de cada variavel na CP



#---------------- BIPLOT
require(bpca)

#####TODAS AS QUANTI
# method of factorization: `hj' to HJ (simetric, Galindo (1986));
y_hj<- bpca(w, method='hj'); names(y_hj); round(y_hj$eigenvalues,4)
?bpca
summary(y_hj)
plot(y_hj, obj.names=F, xlim = c(-4,4), ylim = c(-4,4))

####QUESTIONARIOS
v_hj<- bpca(v, method='hj'); names(v_hj); round(v_hj$eigenvalues,4)
summary(v_hj)
plot(v_hj, obj.names=F)

####EXAMES
x_hj<- bpca(x, method='hj'); names(x_hj); round(x_hj$eigenvalues,4)
summary(x_hj)
plot(x_hj, obj.names=F)


### Biplot por grupos
plot(bpca(w, method='hj'), lwd=1, xlim=c(-10,6),
     var.cex=.6,  obj.names=FALSE, obj.cex=.6, var.factor=1,
     obj.col=c(1:9)[unclass(DIC$trat)],
     obj.pch=c(1:9)[unclass(factor(DIC$trat))])
legend(-10,6, legend=c(
  "p1 x s1","p1 x s2","p1 x s3", 
  "p2 x s1","p2 x s2","p2 x s3", 
  "p3 x s1","p3 x s2","p2 x s3"),
  col=c(1:9), pch=c(1:9), cex=0.5)
