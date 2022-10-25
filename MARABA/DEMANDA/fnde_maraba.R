# Compras locais do PNAE - Marabá
# Objetivo 2: investigar a demanda de alimentos da agricultura familiar de Marabá pela prefeitura de Marabá para o PNAE
  ## Quais alimentos são demandados da AF Marabá
  ## Quantos % são demandados de Marabá?
  ## Quais alimentos são demandados de outras cidades? Estes alimentos são produzidos por Marabá?

library(readxl)
library(dplyr)

# Dados Marabá - cruzar dados fnde com divisão regional do BR 
fnde_maraba=read_excel("C:/Users/Yasmin/Documents/NEPA-TCC/fnde_maraba.xlsx")
colnames(fnde_maraba)
View(fnde_maraba)

#1. Cruzamento: eexgeocod x CD_GEOCODI
colnames(ibge_regional)
ibge_regional=ibge_regional%>%rename("eexgeocod"="geocodigo_fornec")

fnde_maraba_001=merge(fnde_maraba,ibge_regional, by="eexgeocod")
colnames(fnde_maraba_001)
fnde_maraba_001=fnde_maraba_001%>%rename("eex_cod_rgi"="cod_rgi", "eex_nome_rgi"="nome_rgi", "eex_cod_rgint"="cod_rgint", "eex_nome_rgint"="nome_rgint")
View(fnde_maraba_001)

is.na(fnde_maraba_001$eex_cod_rgi)

#2. Cruzamento: fornecgeocod x CD_GEOCODI
colnames(ibge_regional)
ibge_regional=ibge_regional%>%rename("fornecgeocod"="eexgeocod")

fnde_maraba_002=merge(fnde_maraba_001, ibge_regional, by="fornecgeocod")
colnames(fnde_maraba_002)
View(fnde_maraba_002)

fnde_maraba_002=fnde_maraba_002%>%rename("fornec_cod_rgi"="cod_rgi", "fornec_nome_rgi"="nome_rgi", "fornec_cod_rgint"="cod_rgint", "fornec_nome_rgint"="nome_rgint")

colnames(fnde_maraba_002)
fnde_maraba_003=fnde_maraba_002%>%select(1,2,5,6,7,8,9,10,11,13,14,15,16,18,19,20,21)
colnames(fnde_maraba_003)
View(fnde_maraba_003)

#3. Cruzamento: eexgeocod x CODMunic
colnames(geocod_uf)
geocod_uf=geocod_uf%>%select(1,2)
colnames(geocod_uf)=c("eexgeocod","eex_uf")
colnames(geocod_uf)

fnde_maraba_004=merge(fnde_maraba_003, geocod_uf, by="eexgeocod")
colnames(fnde_maraba_004)
View(fnde_maraba_004)

#4. Cruzamento: fornecgeocod x CODMunic
colnames(geocod_uf)=c("fornecgeocod", "fornec_uf")
colnames(geocod_uf)

fnde_maraba_005=merge(fnde_maraba_004, geocod_uf, by="fornecgeocod")
colnames(fnde_maraba_005)
unique(fnde_maraba_005$eexgeocod)
fnde_maraba_005=fnde_maraba_005%>%filter(eexgeocod==1504208)
unique(fnde_maraba_005$eexgeocod)
#####
##Ajeitar as classes:
class(fnde_maraba_005$sum)
fnde_maraba_005$sum=as.numeric(fnde_maraba_005$sum)

######

fnde_maraba_13=fnde_maraba_005%>%filter(ano==2013)
fnde_maraba_14=fnde_maraba_005%>%filter(ano==2014)
fnde_maraba_15=fnde_maraba_005%>%filter(ano==2015)
fnde_maraba_16=fnde_maraba_005%>%filter(ano==2016)
fnde_maraba_17=fnde_maraba_005%>%filter(ano==2017)
fnde_maraba_18=fnde_maraba_005%>%filter(ano==2018)
fnde_maraba_19=fnde_maraba_005%>%filter(ano==2019)
fnde_maraba_20=fnde_maraba_005%>%filter(ano==2020)
class(fnde_maraba_20$sum)

## Demanda do município de maraba
fnde_maraba_maraba = fnde_maraba_005%>%filter(fornecgeocod==eexgeocod)

### 2013
fnde_m_m_13=fnde_maraba_maraba%>%filter(ano==2013)
nrow(fnde_m_m_13)
percent_m_m_13=((sum(fnde_m_m_13$sum))/(sum(fnde_maraba_13$sum)))*100
percent_m_m_13

### 2014
fnde_m_m_14=fnde_maraba_maraba%>%filter(ano==2014)
nrow(fnde_m_m_14)
percent_m_m_14=((sum(fnde_m_m_14$sum))/(sum(fnde_maraba_14$sum)))*100
percent_m_m_14

### 2015
fnde_m_m_15=fnde_maraba_maraba%>%filter(ano==2015)
nrow(fnde_m_m_15)
percent_m_m_15=((sum(fnde_m_m_15$sum))/(sum(fnde_maraba_15$sum)))*100
percent_m_m_15

### 2016
fnde_m_m_16=fnde_maraba_maraba%>%filter(ano==2016)
nrow(fnde_m_m_16)
percent_m_m_16=((sum(fnde_m_m_16$sum))/(sum(fnde_maraba_16$sum)))*100
percent_m_m_16

### 2017
fnde_m_m_17=fnde_maraba_maraba%>%filter(ano==2017)
nrow(fnde_m_m_17)
percent_m_m_17=((sum(fnde_m_m_17$sum))/(sum(fnde_maraba_17$sum)))*100
percent_m_m_17

### 2018
fnde_m_m_18=fnde_maraba_maraba%>%filter(ano==2018)
percent_m_m_18=((sum(fnde_m_m_18$sum))/(sum(fnde_maraba_18$sum)))*100
percent_m_m_18

### 2019
fnde_m_m_19=fnde_maraba_maraba%>%filter(ano==2019)
percent_m_m_19=((sum(fnde_m_m_19$sum))/(sum(fnde_maraba_19$sum)))*100
percent_m_m_19

### 2020
fnde_m_m_20=fnde_maraba_maraba%>%filter(ano==2020)
percent_m_m_20=((sum(fnde_m_m_20$sum))/(sum(fnde_maraba_20$sum)))*100
percent_m_m_20

#####
# Dataset
#####
vetor_percent_m_m=c(percent_m_m_13,percent_m_m_14,percent_m_m_15, percent_m_m_16, percent_m_m_17, percent_m_m_18, percent_m_m_19, percent_m_m_20)
matriz_percent_maraba_maraba=matrix(data=vetor_percent_m_m, byrow = TRUE)
matriz_percent_maraba_maraba

vector_anos=c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
matriz_anos=matrix(data=vector_anos, byrow = TRUE)

data_percent_maraba_maraba=cbind(matriz_anos,matriz_percent_maraba_maraba)
View(data_percent_maraba_maraba)
colnames(data_percent_maraba_maraba)=c("Ano","Percentual adquirido do Mun. de Marabá")

## Demanda da região geográfica imediata
fnde_maraba_rgi=fnde_maraba_005%>%filter(fornec_cod_rgi==eex_cod_rgi)
nrow(fnde_maraba_rgi)

### 2013
fnde_m_rgi_13=fnde_maraba_rgi%>%filter(ano==2013)
percent_m_rgi_13=((sum(fnde_m_rgi_13$sum))/(sum(fnde_maraba_13$sum)))*100
percent_m_rgi_13

### 2014
fnde_m_rgi_14=fnde_maraba_rgi%>%filter(ano==2014)
percent_m_rgi_14=((sum(fnde_m_rgi_14$sum))/(sum(fnde_maraba_14$sum)))*100
percent_m_rgi_14

### 2015
fnde_m_rgi_15=fnde_maraba_rgi%>%filter(ano==2015)
percent_m_rgi_15=((sum(fnde_m_rgi_15$sum))/(sum(fnde_maraba_15$sum)))*100
percent_m_rgi_15

### 2016
fnde_m_rgi_16=fnde_maraba_rgi%>%filter(ano==2016)
percent_m_rgi_16=((sum(fnde_m_rgi_16$sum))/(sum(fnde_maraba_16$sum)))*100
percent_m_rgi_16

### 2017
fnde_m_rgi_17=fnde_maraba_rgi%>%filter(ano==2017)
percent_m_rgi_17=((sum(fnde_m_rgi_17$sum))/(sum(fnde_maraba_17$sum)))*100
percent_m_rgi_17

### 2018
fnde_m_rgi_18=fnde_maraba_rgi%>%filter(ano==2018)
percent_m_rgi_18=((sum(fnde_m_rgi_18$sum))/(sum(fnde_maraba_18$sum)))*100
percent_m_rgi_18

### 2019
fnde_m_rgi_19=fnde_maraba_rgi%>%filter(ano==2019)
percent_m_rgi_19=((sum(fnde_m_rgi_19$sum))/(sum(fnde_maraba_19$sum)))*100
percent_m_rgi_19

### 2020
fnde_m_rgi_20=fnde_maraba_rgi%>%filter(ano==2020)
percent_m_rgi_20=((sum(fnde_m_rgi_20$sum))/(sum(fnde_maraba_20$sum)))*100
percent_m_rgi_20

#####
# Dataset
#####
vetor_percent_m_rgi=c(percent_m_rgi_13,percent_m_rgi_14,percent_m_rgi_15, percent_m_rgi_16, percent_m_rgi_17, percent_m_rgi_18, percent_m_rgi_19, percent_m_rgi_20)
matriz_percent_maraba_rgi=matrix(data=vetor_percent_m_rgi, byrow = TRUE)
matriz_percent_maraba_rgi

vector_anos=c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
matriz_anos=matrix(data=vector_anos, byrow = TRUE)

data_percent_maraba_rgi=cbind(matriz_anos,matriz_percent_maraba_rgi)
View(data_percent_maraba_rgi)
colnames(data_percent_maraba_rgi)=c("Ano","Percentual adquirido da RGI Marabá")

## Demanda da região geográfica intermediária
fnde_maraba_rgint=fnde_maraba_005%>%filter(fornec_cod_rgint==eex_cod_rgint)
nrow(fnde_maraba_rgint)

### 2018
fnde_m_rgint_18=fnde_maraba_rgint%>%filter(ano==2018)
percent_m_rgint_18=((sum(fnde_m_rgint_18$sum))/(sum(fnde_maraba_18$sum)))*100
percent_m_rgint_18
  
### 2019
fnde_m_rgint_19=fnde_maraba_rgint%>%filter(ano==2019)
percent_m_rgint_19=((sum(fnde_m_rgint_19$sum))/(sum(fnde_maraba_19$sum)))*100
percent_m_rgint_19

### 2020
fnde_m_rgint_20=fnde_maraba_rgint%>%filter(ano==2020)
percent_m_rgint_20=((sum(fnde_m_rgint_20$sum))/(sum(fnde_maraba_20$sum)))*100
percent_m_rgint_20

#####
# Dataset
#####
vetor_percent_m_rgint=c(percent_m_rgi_13,percent_m_rgi_14,percent_m_rgi_15, percent_m_rgi_16, percent_m_rgi_17, percent_m_rgint_18, percent_m_rgint_19, percent_m_rgint_20)
matriz_percent_maraba_rgint=matrix(data=vetor_percent_m_rgint, byrow = TRUE)
matriz_percent_maraba_rgint

vector_anos=c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
matriz_anos=matrix(data=vector_anos, byrow = TRUE)

data_percent_maraba_rgint=cbind(matriz_anos,matriz_percent_maraba_rgint)
View(data_percent_maraba_rgint)
colnames(data_percent_maraba_rgint)=c("Ano","Percentual adquirido da RGINT Marabá")

## Demanda da UF (PARÁ)
colnames(fnde_maraba_005)
fnde_maraba_uf=fnde_maraba_005%>%filter(fornec_uf==eex_uf)
nrow(fnde_maraba_uf)

### 2018
fnde_m_uf_18=fnde_maraba_uf%>%filter(ano==2018)
percent_m_uf_18=((sum(fnde_m_uf_18$sum))/(sum(fnde_maraba_18$sum)))*100
percent_m_uf_18

### 2019
fnde_m_uf_19=fnde_maraba_uf%>%filter(ano==2019)
percent_m_uf_19=((sum(fnde_m_uf_19$sum))/(sum(fnde_maraba_19$sum)))*100
percent_m_uf_19

#####
# Dataset
#####
vetor_percent_m_uf=c(percent_m_rgi_13,percent_m_rgi_14,percent_m_rgi_15, percent_m_rgi_16, percent_m_rgi_17, percent_m_uf_18, percent_m_uf_19, percent_m_rgint_20)
matriz_percent_maraba_uf=matrix(data=vetor_percent_m_uf, byrow = TRUE)
matriz_percent_maraba_uf

vector_anos=c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
matriz_anos=matrix(data=vector_anos, byrow = TRUE)

data_percent_maraba_uf=cbind(matriz_anos,matriz_percent_maraba_uf)
View(data_percent_maraba_uf)
colnames(data_percent_maraba_uf)=c("Ano","Percentual adquirido do Estado do Pará")

## Quais alimentos são demandados da AF Maraba?
#compras locais:
colnames(fnde_maraba_maraba)
View(fnde_maraba_maraba)
fnde_maraba_maraba_item=fnde_maraba_maraba%>%group_by(item)%>%summarise(sum(sum))
View(fnde_maraba_maraba_item)

fnde_m_m_frutas=fnde_maraba_maraba_item[c(1,2,3,14,15,16,17,18,49,54,55,61,62,63,64,75,76),]
percent_m_m_frutas=((sum(fnde_m_m_frutas$`sum(sum)`))/(sum(fnde_maraba_maraba_item$`sum(sum)`)))
percent_m_m_frutas

fnde_m_m_hortalicas=fnde_maraba_maraba_item[c(4,5,6,7,8,10,11,12,13,19,20,21,24,25,26,27,28,29,30,31,32,33,34,41,42,43,44,45,46,47,48,52,53,56,58,59,60,65,68),]
percent_m_m_hortalicas=((sum(fnde_m_m_hortalicas$`sum(sum)`))/(sum(fnde_maraba_maraba_item$`sum(sum)`)))
percent_m_m_hortalicas

fnde_m_m_ovos=fnde_maraba_maraba_item[c(66),]
percent_m_m_ovos=((sum(fnde_m_m_ovos$`sum(sum)`))/(sum(fnde_maraba_maraba_item$`sum(sum)`)))
percent_m_m_ovos

fnde_m_m_conservas=fnde_maraba_maraba_item[c(9,22,23,35,50,57,67,69,70,71,72,74),]
percent_m_m_conservas=((sum(fnde_m_m_conservas$`sum(sum)`))/(sum(fnde_maraba_maraba_item$`sum(sum)`)))
percent_m_m_conservas

fnde_m_m_derivados=fnde_maraba_maraba_item[c(36,37,38,39,40,51,73),]
percent_m_m_derivados=((sum(fnde_m_m_derivados$`sum(sum)`))/(sum(fnde_maraba_maraba_item$`sum(sum)`)))
percent_m_m_derivados

sum(percent_m_m_frutas, percent_m_m_hortalicas, percent_m_m_ovos, percent_m_m_conservas, percent_m_m_derivados)

#compras de outras cidades:
colnames(fnde_maraba_005)
fnde_maraba_outras = fnde_maraba_005%>%filter(fornecgeocod!=eexgeocod)
nrow(fnde_maraba_005)
nrow(fnde_maraba_maraba)
nrow(fnde_maraba_outras)

fnde_maraba_outras_item=fnde_maraba_outras%>%group_by(item)%>%summarise(sum(sum))
View(fnde_maraba_outras_item)

fnde_m_outras_hortalicas=fnde_maraba_outras_item[c(1,2,4,7,8,9),]
percent_m_outras_hortalicas=((sum(fnde_m_outras_hortalicas$`sum(sum)`))/(sum(fnde_maraba_outras_item$`sum(sum)`)))
percent_m_outras_hortalicas

fnde_m_outras_frutas=fnde_maraba_outras_item[c(12),]
percent_m_outras_frutas=((sum(fnde_m_outras_frutas$`sum(sum)`))/(sum(fnde_maraba_outras_item$`sum(sum)`)))
percent_m_outras_frutas

fnde_m_outras_derivados=fnde_maraba_outras_item[c(6),]
percent_m_outras_derivados=((sum(fnde_m_outras_derivados$`sum(sum)`))/(sum(fnde_maraba_outras_item$`sum(sum)`)))
percent_m_outras_derivados

fnde_m_outras_conservas=fnde_maraba_outras_item[c(3,10,11),]
percent_m_outras_conservas=((sum(fnde_m_outras_conservas$`sum(sum)`))/(sum(fnde_maraba_outras_item$`sum(sum)`)))
percent_m_outras_conservas

fnde_m_outras_ind=fnde_maraba_outras_item[c(5),]
percent_m_outras_ind=((sum(fnde_m_outras_ind$`sum(sum)`))/(sum(fnde_maraba_outras_item$`sum(sum)`)))
percent_m_outras_ind

sum(percent_m_outras_frutas, percent_m_outras_hortalicas, percent_m_outras_derivados, percent_m_outras_conservas, percent_m_outras_ind)

library(writexl)
write_xlsx(fnde_maraba_maraba_item, 'alimentos_demanda_maraba_maraba.xlsx')
write_xlsx(fnde_maraba_outras_item, 'alimentos_demanda_maraba_outras.xlsx')
