###===###===###===###===###===###===###===###===###===###===###===###
### Manuella (Couve) - HU UFSC
###===###===###===###===###===###===###===###===###===###===###===###
# http://leg.ufpr.br/~paulojus/CE223/ce223/ce223se7.html

library(foreign)
require(foreign)
dados<- read.spss("Banco HU UFSC sem nomes 2019.09.20.sav", head=T); head(dados)
str(dados)
names(dados)

# A maioria das escalas tem uma pontua??o cont?nua (d? para fazer correla??es com 
# os marcadores).. mas tamb?m pontes de corte determinando se o paciente tem o n?o
# tem o sintoma.  Eu particularmente me interesso bastante por trauma na inf?ncia 
# e stress tb (QUESI).. pois ? um fator bem forte para determinar o surgimento dos
# transtornos psiqui?tricos.  
                                             
dados[c(36,45),]
class(dados)
dados

paciente <- dados$Numero_paciente; paciente

dados$TCLE
levels(termo)
for(i in 1:51){
  if(dados$TCLE[i] == "SIM"){
    dados$TCLE[i] <- "sim"
  }
}
termo <- dados$TCLE; termo
summary(termo); class(termo)
termo <- as.factor(termo)

#dados$Telefones_de_contato

dados$Datacoleta   #arrumar
data_coleta <- as.POSIXct(dados$Datacoleta, origin="1582-10-14", tz="GMT"); data_coleta


idade <- dados$Idade

#dados$Sexo

hemacias <- dados$hemacias
hemoglobina <- dados$hemoglobina
hematocrito <- dados$hematocrito
volumeglob <- dados$volglob #volume corpuscular m?dio
hemoglobina_m <- dados$hemglobm #hemoglobina corpuscular m?dia
hemoglobina_concent <- dados$chglob #concentracao homoglobina
rdw <- dados$RDW #variabilidade de eritrocitos

leucocitos <- dados$Leucocitos
segmentados <- dados$segmentados
segmentados_m <- dados$segmentadosmm
bastonados <- dados$bastonados
linfocitos <- dados$linfocitos
linfocitos_m <-dados$linfocitosmm
monocitos <- dados$monocitos
monocitos_m <- dados$monocitosmm
eosinofilos <- dados$eosinofilos
eosinofilos_m <- dados$eosinofilosmm
basofilos <- dados$basofilos
basofilos_m <-dados$basofilosmm

plaquetas <- dados$plaquetas
plaquetas_vol <- dados$MPV; plaquetas_vol

#######VAO REFAZER####################################
#dados$PCR
#cortisol
#CRH
#IL-6
#TNF-alfa
#IL-10. 
######################################################

#######DATA
dados$Data_avalia??o_psiquiatria #arrumar
data_avaliacao <- as.POSIXct(dados$Data_avalia??o_psiquiatria, origin="1582-10-14", tz="GMT")


##dados$Prontuario_HU

##dados$Hora_coleta_sangue

##dados$Hora_coleta_saliva

##dados$Cidade_onde_nasceu#36 e 45 n?o tem aqui

dados$Data_de_nascimento#36 e 45 n?o tem aqui #arrumar
data_nasc <- as.POSIXct(dados$Data_de_nascimento, origin="1582-10-14", tz="GMT"); data_nasc

dados$Idade_psiquiatria #36 e 45 n?o tem aqui
dados$Sexo_psiquiatria #36 e 45 n?o tem aqui

sexo <- dados$Sexo
sexo[51] <- "mulheres"; sexo
sexo <- ifelse(sexo == "mulheres", "feminino", "masculino"); sexo
sexo <- as.factor(sexo); sexo

cor_raca <- dados$Cor_ou_ra?a #36 e 45 n?o tem aqui

factor(dados$Estado_civil);levels(dados$Estado_civil) #36 e 45 n?o tem aqui
#solteiro, divorciado, viuvo, casado
est_civil <- ifelse(dados$Estado_civil == "solteiro/nunca casou", "solteiro",
       ifelse(dados$Estado_civil == "divorciado/desquitado/separado", "divorciado",
        ifelse(dados$Estado_civil == "vi?vo(a)", "viuvo",
         ifelse(dados$Estado_civil == "casado/amasiado(a)", "casado", NA))))
est_civil


tempo_casado <- dados$Tempo_de_casado; tempo_casado #NA est? como n?o se aplica #36 e 45 n?o tem aqui
tempo_casado <- ifelse(is.na(dados$Tempo_de_casado), 0, dados$Tempo_de_casado); 
tempo_casado[c(36,45)] <- NA; tempo_casado
####N?o se aplica como 0

num_filhos <- dados$N?mero_de_filhos #NA est? como n?o se aplica #36 e 45 n?o tem aqui
num_filhos <- ifelse(is.na(dados$N?mero_de_filhos), 0, dados$N?mero_de_filhos); num_filhos
num_filhos[c(36,45)] <- NA; num_filhos
####N?o se aplica como 0

escolaridade <- factor(dados$Qual_a_sua_escolaridade); levels(dados$Qual_a_sua_escolaridade) #36 e 45 n?o tem aqui
escolaridade
###Deixei a mesma classifica??o

anos_estudo <- dados$Anos_de_estudo #36 e 45 n?o tem aqui
anos_estudo
####NAs --- missing 

estuda_atualmente <- dados$Estudo_atual; estuda_atualmente #36 e 45 n?o tem aqui

trabalha_atualmente <- dados$Trabalho_atual #36 e 45 n?o tem aqui

ocupacao <- factor(dados$Ocupa??o_atual); levels(dados$Ocupa??o_atual) #36 e 45 n?o tem aqui
ocupacao
#trabalho informal, trabalho formal, domestico, nao se aplica
ocupacao <- ifelse(dados$Ocupa??o_atual == "trabalha formalmente/ carteira assinada", "trabalho formal",
                   ifelse(dados$Ocupa??o_atual == "trabalha informalmente/ bicos", "trabalho informal",
                          ifelse(dados$Ocupa??o_atual == "do lar", "dom?stico",
                              ifelse(dados$Ocupa??o_atual == "n?o se aplica", "nao se aplica", NA))))
ocupacao
ocupacao[c(44, 40, 25, 23)] <- "nao se aplica"; ocupacao

#nao se aplica: 44, 40, 25, 23
#missing 43

ocupacao_qual <- dados$Qual_ocupa??o_atual #36 e 45 n?o tem aqui

#lar: 7, 28, 48, 49
#nao se aplica: 18, 19, 30, 38, 44
#missing: 41

ocupacao_qual[c(7,28,48,49)] <- "do lar"
ocupacao_qual[c(18,19,30,38,44)] <- "nao se aplica"
ocupacao_qual

renda <- dados$Renda_familiar; renda #36 e 45 n?o tem aqui
#NA ? missing

quant_pessoas <- dados$N?mero_pessoas_moram_na_casa; quant_pessoas #36 e 45 n?o tem aqui
#NA ? missing

religiao_import <- factor(dados$Religi?o_import?ncia); levels(dados$Religi?o_import?ncia)#36 e 45 n?o tem aqui
religiao_import

religiao_freq <- factor(dados$Religi?o_frequ?ncia); levels(dados$Religi?o_frequ?ncia)#36 e 45 n?o tem aqui
religiao_freq

tabagismo <- dados$Tabagismo; tabagismo#36 e 45 n?o tem aqui
#missing 30, 31, 34, 51

tabagismo_cigarros <- dados$Tabagismo_cigarros_semana; tabagismo_cigarros#36 e 45 n?o tem aqui
##E tbm: 0 est? sendo representado por NA
##Tem missing mas ta embaralhado com NA
#missing: 6, 30, 31, 34, 44, 51
tabagismo_cigarros <- ifelse(is.na(dados$Tabagismo_cigarros_semana), 0, dados$Tabagismo_cigarros_semana); tabagismo_cigarros
tabagismo_cigarros[c(6,30,31,34,44,51,36,45)] <- NA; tabagismo_cigarros

tabagismo_tempo <- dados$Tempo_de_tabagismo; tabagismo_tempo #36 e 45 n?o tem aqui
##E tbm: nao se aplica est? sendo representado por NA
##Tem missing mas ta embaralhado com NA
#missing: 30, 31, 34, 51
tabagismo_tempo <- ifelse(is.na(dados$Tempo_de_tabagismo), 0, dados$Tempo_de_tabagismo);tabagismo_tempo
tabagismo_tempo[c(30,31,34,51,36,45)] <- NA; tabagismo_tempo

exercicio_fis <- dados$Exerc?cio_regular; exercicio_fis#36 e 45 n?o tem aqui
#missing 30, 31, 34, 51

exercicio_tempo_semana <- dados$Exerc?cio_horas_semana; exercicio_tempo#36 e 45 n?o tem aqui
##E tbm: nao se aplica representado por NA
#missing 30, 31, 34, 51
exercicio_tempo_semana <- ifelse(is.na(dados$Exerc?cio_horas_semana), 0, dados$Exerc?cio_horas_semana); exercicio_tempo
exercicio_tempo_semana[c(30,31,34,51,36,45)] <- NA;exercicio_tempo_semana

exercicio_duracao <- dados$Tempo_exerc?cio_regular#36 e 45 n?o tem aqui
##E tbm: nao se aplica representado por NA
#missing: nenhum?
exercicio_duracao <- ifelse(is.na(dados$Tempo_exerc?cio_regular), 0, dados$Tempo_exerc?cio_regular); exercicio_duracao
exercicio_duracao[c(36,45)] <- NA; exercicio_duracao

medicamentos_uso <- dados$Medicamentos_em_uso#36 e 45 n?o tem aqui###################
####ARRUMAR

#####dados$MEDIC0##???##########################

antipsicoticos <- dados$Usa_antipsic?tico#36 e 45 n?o tem aqui

doencas_diag <- dados$Doen?as_diagnosticadas#36 e 45 n?o tem aqui#############################
##E tbm: nao se aplica representado por espa?o em branco
#####ARRUMAR

traumatismo <- dados$Traumatismo_craniano#36 e 45 n?o tem aqui

traumatismo_tempo <- dados$Traumatismo_craniano_tempo#36 e 45 n?o tem aqui  ESTA EM ANOS
##E tbm: nao se aplica representado por NA
traumatismo_tempo <- ifelse(is.na(dados$Traumatismo_craniano_tempo), 0, dados$Traumatismo_craniano_tempo); traumatismo_tempo
traumatismo_tempo[c(36,45)] <- NA; traumatismo_tempo

traumatismo_perda_consc <- dados$Traumatismo_craniano_perda_consci?ncia; 
#36 e 45 n?o tem aqui
traumatismo_perda_consc
traumatismo_perda_consc[c(22,30,31,32)] <- "n?o se aplica"

peso <- dados$Peso#36 e 45 n?o tem aqui

altura <- dados$Altura#36 e 45 n?o tem aqui

internacao_psic <- dados$Interna??o_psiqui?trica#36 e 45 n?o tem aqui

internacao_quant <-dados$N?mero_interna??es_psiqui?tricas#36 e 45 n?o tem aqui
internacao_quant <- ifelse(is.na(dados$N?mero_interna??es_psiqui?tricas), 0, dados$N?mero_interna??es_psiqui?tricas); internacao_quant
internacao_quant[c(36,45)] <- NA; internacao_quant
##E tbm: nao se aplica representado por NA

internacao_ultima <- dados$Ano_?ltima_interna??o_psiqui?trica#36 e 45 n?o tem aqui
##E tbm: nao se aplica representado por NA
internacao_ultima <- ifelse(is.na(dados$Ano_?ltima_interna??o_psiqui?tric), 0, dados$Ano_?ltima_interna??o_psiqui?tric); internacao_ultima
internacao_ultima[c(36,45,30)] <- NA; internacao_ultima
#30 ? missing

internacao_motivo <- dados$Motivo_?lltima_interna??o#36 e 45 n?o tem aqui
##E tbm: nao se aplica esta em branco
#missing: 14, 30
internacao_motivo[c(36,45,14,30)] <- NA; internacao_motivo

internacao_geral <- dados$Interna??o_geral#36 e 45 n?o tem aqui
##missing: 30, 44
internacao_geral

internacao_geral_quant <- dados$N?mero_interna??o_geral#36 e 45 n?o tem aqui
##E tbm: nao se aplica como NA, missing embaralhado com NA
#missing: 10, 30, 38, 41, 44
internacao_geral_quant <- ifelse(is.na(dados$N?mero_interna??o_geral), 0, dados$N?mero_interna??o_geral); internacao_geral_quant
internacao_geral_quant[c(10,30,38,41,44,36,45)] <- NA; internacao_geral_quant

internacao_geral_ultima <- dados$Ano_?ltima_interna??o_geral#36 e 45 n?o tem aqui
##E tbm: nao se aplica como NA
#missing: 30, 44
internacao_geral_ultima <- ifelse(is.na(dados$Ano_?ltima_interna??o_geral), 0, dados$Ano_?ltima_interna??o_geral); internacao_geral_ultima
internacao_geral_ultima[c(30,44,36,45)] <- NA; internacao_geral_ultima

internacao_geral_ultima_motivo <- dados$Motivo_?ltima_interna??o_geral#36 e 45 n?o tem aqui
##E tbm: nao se aplica como espa?o em branco
#missing: 30, 44
internacao_geral_ultima_motivo[c(30,44,36,45)] <- NA; internacao_geral_ultima_motivo
internacao_geral_ultima_motivo[c(1,3,4,22,25,28,29,31)] <- "nao se aplica"; internacao_geral_ultima_motivo

####MINI#########

dados$Epis?dio_Depressivo_Maior_atual

dados$Epis?dio_Depressivo_Maio_passado

dados$EDM_com_caracter?sticas_melanc?licas

dados$Distimia #falta 1, 4, 8, 9, 12, 16, 30, 35, 39, 51
dados$Epis?dio_hipo_man?aco #falta dados na 1 e 23

dados$EDM_triste
dados$EDM_falta_de_prazer
dados$EDM_A3a_apetite
dados$EDM_A3b_sono
dados$EDM_A3c_lentificado_agitado
dados$EDM_A3a_apetite
dados$EDM_A3d_anergia
dados$EDM_A3e_culpa
dados$EDM_A3f_tomar_decis?es
dados$EDM_A3g_pensamentos_de_morte

dados$EDM_passado
dados$EDM_caracter?sticas_melanc?licas

dados$Risco_de_suicidio 
dados$Risco_de_suicidio_grau #NULL
dados$Risco_de_suicidio_C1 #falta 10, 42
dados$Risco_de_suicidio_C2 #falta 10, 42
dados$Risco_de_suicidio_C3 #falta 10, 42
dados$Risco_de_suicidio_C4 #falta 10, 42
dados$Risco_de_suicidio_C5 #falta 10, 42
dados$Risco_de_suicidio_C6 #falta 10, 33, 34, 42 

dados$Epis?dio_hipoman?aco #falta 18, 20, 27
dados$Epis?dio_hipoman?aco_atual_passado #falta 18, 23, 30
dados$Epis?dio_man?aco #falta 4
dados$Epis?dio_man?aco_atual_passado #falta 4, 23

dados$Transtorno_de_p?nico #falta 14

dados$Agorafobia

dados$Fobia_social

dados$Transtorno_obsessivo_compulsivo

dados$TEPT

dados$Abuso_de_?lcool
dados$Depend?ncia_de_?lcool
dados$Abuso_de_?lcool
dados$Qual_subst?ncia_abuso
dados$Depend?ncia_subst?ncia
dados$Qual_subst?ncia_depend?ncia
dados$S?ndrome_psic?tica_atual
dados$S?ndrome_psic?tica_vida_inteira
dados$Transtorno_do_humor_sint_psic?ticos_atual
dados$Transtorno_do_humor_sint_psic?ticos_vida_inteira
dados$Anorexia_nervosa
dados$Bulimia_nervosa
dados$Transtorno_ansiedade_generalizada
dados$TPAS_antes_dos_15
dados$Transtorno_personalidade_antisocial


#########################################FIM QUESTIONARIO SOCIODEMOGR?FICO

##########################BANCO DE DADOS NOVO######################
library(tidyverse)
###CHAMANDO
write.csv(bd, file = "novo_hu")

bd <- read.csv("C:/R/novo_hu"); head(bd)
tail(bd)
bd$X.1 <- NULL
names(bd)

##ADICIONANDO COLUNAS NOVAS

bd <- mutate(bd, panas_neg, panas_pos, hads_total, hads_ans, hads_dep, neuroticismo); 
head(bd)
tail(bd)
names(bd)
bd$X.2 <- NULL
bd$X <- NULL
bd$hads <- NULL

###########################
#COLOQUEI COMO 0 (zero) EM VARIAS VAR PRA REPRESENTAR O nao se aplica
#PORQUE ELAS SAO NUMERICAS, E DAI IA FICAR COMO CARACTERE 
##########################BANCO DE DADOS NOVO######################


##SHAPS#################OKK
shaps <- dados$SHAPS_C_total; shaps
shaps1 <- dados$SHAPS_C_1
levels(shaps1)
shaps1 <- ifelse(shaps1 == "sente muito prazer", 1,
                 ifelse(shaps1 == "prazer m?dio ou habitual", 2,
                        ifelse(shaps1 == "sente algum prazer", 3,
                               ifelse(shaps1 == "n?o sente prazer", 4, NA)))); shaps1

shaps8 <- dados$SHAPS_C_8
shaps8 <- ifelse(shaps8 == "sente muito prazer", 1,
                 ifelse(shaps8 == "prazer m?dio ou habitual", 2,
                        ifelse(shaps8 == "sente algum prazer", 3,
                               ifelse(shaps8 == "n?o sente prazer", 4, NA)))); shaps8


##ACIPS################OKK
acips_total <- dados$ACIPS_total
acips_total <- acips_antec + acips_cons; acips_total
#missing: 15, 37, 39 e 47
acips_antec <- dados$ACIPS_antecipat?rio
#Manu: 49 dados (falta 36 e 45)
#missing: 15 e 39
acips_cons <- dados$ACIPS_consumat?rio
#missing: 37, 39 e 47

acips1 <- dados$ACIPS_1
acips1 <- ifelse(acips1 == "muito falso para mim", 1, 
       ifelse(acips1 == "moderadamento falso para mim", 2,
              ifelse(acips1 == "um pouco falso para mim", 3,
                     ifelse(acips1 == "um pouco verdadeiro para mim", 4,
                            ifelse(acips1 == "moderadamente verdadeiro para mim", 5, 6)))))
acips1

acips8 <- dados$ACIPS_8
acips8 <- ifelse(acips8 == "muito falso para mim", 1, 
                 ifelse(acips8 == "moderadamento falso para mim", 2,
                        ifelse(acips8 == "um pouco falso para mim", 3,
                               ifelse(acips8 == "um pouco verdadeiro para mim", 4,
                                      ifelse(acips8 == "moderadamente verdadeiro para mim", 5, 6)))))
acips8

acips17 <- dados$ACIPS_17
acips17 <- ifelse(acips17 == "muito falso para mim", 1, 
                  ifelse(acips17 == "moderadamento falso para mim", 2,
                         ifelse(acips17 == "um pouco falso para mim", 3,
                                ifelse(acips17 == "um pouco verdadeiro para mim", 4,
                                       ifelse(acips17 == "moderadamente verdadeiro para mim", 5, 6)))))
acips17

##TEPS####################OKK
teps_cons <- dados$TEPS_consumat?rio; teps_cons #falta 36 e 45
#missing: 39
teps_antec <- dados$TEPS_antecipat?rio; teps_antec
#missing: 7, 18, 39
teps_total <- dados$TEPS_total; teps_total
teps_total <- teps_antec + teps_cons; teps_total
#missing: 7, 18, 39

teps1 <- dados$TEPS_1
teps1 <- ifelse(teps1 == "muito falso para mim", 1, 
                ifelse(teps1 == "moderadamento falso para mim", 2,
                       ifelse(teps1 == "um pouco falso para mim", 3,
                              ifelse(teps1 == "um pouco verdadeiro para mim", 4,
                                     ifelse(teps1 == "moderadamente verdadeiro para mim", 5,
                                            ifelse(teps1 == "muito verdadeiro para mim", 6, 2))))))
teps1

teps5 <- dados$TEPS_5
teps5 <- ifelse(teps5 == "muito falso para mim", 1, 
                   ifelse(teps5 == "moderadamento falso para mim", 2,
                          ifelse(teps5 == "um pouco falso para mim", 3,
                                 ifelse(teps5 == "um pouco verdadeiro para mim", 4,
                                        ifelse(teps5 == "moderadamente verdadeiro para mim", 5, 
                                               ifelse(teps5 == "muito verdadeiro para mim", 6, 2))))))
teps5

teps13r <- dados$TEPS_13_reverse
teps13r ##ja esta certinha




##BIS/BAS#######################okk

###as questoes nao estao codificadas
###no questionario fala em 1, 2, 3 e 4, ? isso mesmo?
levels(dados$BISBAS_1)
levels(dados$BISBAS_10)

###tudo NA as variaveis totais

bis_total <- dados$BIS_total
#missing: 21, 39 #falta: 36, 45
bis_total <- c(19,20,22,16,21,18,28,25,22,25,24,19,14,22,23,28,13,21,23,22,NA,21,16,26,17,20,
  22,23,26,28,26,28,27,25,22,NA,11,19,NA,26,24,20,21,18,NA,9,21,27,21,19,26); bis_total

bas_total <- dados$BAS_total
#missing: 4, 39 falta: 36, 45

bas_total <- c(46,20,45,NA,45,23,33,42,41,49,40,48,44,37,26,43,32,32,36,28,33,33,22,
               34,25,36,32,39,36,41,38,31,40,32,42,NA,32,39,NA,45,33,34,36,38,NA,39,
               33,28,38,42,42); bas_total


###O CALCULO QUE ENCONTRAMOS FOI:
##padronizando
Z_escoreBIS <- (BIS - mean(bis_total))/sd(bis_total)
Z_escoreBAS <- (BAS - mean(bas_total))/sd(bas_total)
Z_BIS_BAS <- Z_escoreBIS - Z_escoreBAS

###O c?lculo ? esse??? verificar


bisbas2 <- dados$BISBAS_2
bisbas2 <- ifelse(bisbas2 == "muito verdadeiro para mim", 1, 
                  ifelse(bisbas2 == "mais verdadeiro do que falso para mim", 2,
                         ifelse(bisbas2 == "mais falso do que verdadeiro para mim", 3, 4)))
bisbas2

bisbas8 <- dados$BISBAS_8
bisbas8 <- ifelse(bisbas8 == "muito verdadeiro para mim", 1, 
                  ifelse(bisbas8 == "mais verdadeiro do que falso para mim", 2,
                         ifelse(bisbas8 == "mais falso do que verdadeiro para mim", 3, 4)))
bisbas8

bisbas18 <- dados$BISBAS_18
bisbas18 <- ifelse(bisbas18 == "muito verdadeiro para mim", 1, 
                   ifelse(bisbas18 == "mais verdadeiro do que falso para mim", 2,
                          ifelse(bisbas18 == "mais falso do que verdadeiro para mim", 3, 4)))
bisbas18

#Neuroticismo############FALTA CONFERIR 

neuroticismo <- dados$Neuroticismo_total
##como que calcula o escore final?? 

neuro2 <- dados$Neuroticismo_2
levels(neuro2)
neuro2 <- ifelse(neuro2 == "discordo totalmente", 1,
                 ifelse(neuro2 == "discordo em parte", 2,
                        ifelse(neuro2 == "nem concordo nem discordo", 3,
                               ifelse(neuro2 == "concordo em parte", 4,
                                      ifelse(neuro2 == "concordo totalmente", 5, NA))))); neuro2

neuro7 <- dados$Neuroticismo_7_invertido; neuro7

##PANAS#################OKK

dados$PANAS_1
dados$PANAS_2
######as questoes nao estao codificadas

##dados$PANAS_total NAO TEM MAIS O BD
panas_neg <- dados$PANAS_afeto_negativo; panas_neg
panas_neg <- c(16,12,13,27,22,28,31,33,24,26,23,23,30,22,33,36,22,16,14,26,22,17,41,27,
      21,19,22,19,16,32,21,40,32,24,38,NA,27,31,NA,27,16,25,16,25,NA,20,35,30,19,14,34)
panas_neg

panas_pos <- dados$PANAS_afeto_positivo; panas_pos
panas_pos <- c(29,17,32,17,37,34,22,32,31,46,43,46,36,30,23,32,34,34,37,29,33,33,15,23,
               21,43,31,11,40,23,34,23,42,30,36,NA,39,29,NA,41,37,32,43,26,NA,25,23,21,
               43,16,38); panas_pos

panas1 <- dados$PANAS_1; panas1
levels(panas1)
panas1 <- ifelse(panas1 == "nada ou muito pouco", 1,
                 ifelse(panas1 == "um pouco", 2,
                        ifelse(panas1 == "moderado", 3,
                               ifelse(panas1 == "muito", 4,
                                      ifelse(panas1 == "totalmente", 5, NA))))); panas1

panas11 <- dados$PANAS_11
panas11 <- ifelse(panas11 == "nada ou muito pouco", 1,
                  ifelse(panas11 == "um pouco", 2,
                         ifelse(panas11 == "moderado", 3,
                                ifelse(panas11 == "muito", 4,
                                       ifelse(panas11 == "totalmente", 5, NA))))); panas11

###o total ? simplesmente a soma dos dois afetos?
###o escore de cada questao ? mesmo de 1 a 5 como definido pelo questionario?



##HADS#############################OKK

hads_total <- dados$HADS_total; hads_total
hads_dep <- dados$HADS_D_total
hads_ans <- dados$HADS_A_total


##QUESI################FALTA CONFERIR

quesi_total <- dados$QUESI_total ##diferente
quesi_total <- quesi_abuso_emoc + quesi_abuso_fis + quesi_abuso_sex + quesi_neglig_emoc + quesi_neglig_fis; quesi_total

quesi_abuso_emoc <- dados$QUESI_abuso_emocional #ok

quesi_abuso_fis <- dados$QUESI_abuso_fisico #ok
quesi_abuso_fis[44] <- 7; quesi_abuso_fis

quesi_abuso_sex <- dados$QUESI_abuso_sexual #ok

quesi_neglig_fis <- dados$QUESI_negligencia_fisica #td diferente
###questoes 1, 2, 4, 6, 26
dados$QUESI_1

quesi_neglig_emoc <- dados$QUESI_negligencia_emocional #td diferente
###questoes 3, 8, 14, 18, 25




















