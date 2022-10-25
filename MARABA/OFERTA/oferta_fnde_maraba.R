# Vendas locais - Marabá
# Objetivo 2: investigar a oferta de alimentos da agricultura familiar de Marabá para o PNAE
## Quais alimentos são vendidos pela AF Marabá
## Quantos % são ofertados para Marabá?
## Quais alimentos são ofertados para outras cidades? Estes alimentos são demandados pela prefeitura de Marabá?

library(readxl)
library(dplyr)
library(stringi)
library(stringr)

# Dados
#Dados:
fnde_maraba=read_excel("C:/Users/Yasmin/Documents/NEPA-Tmm/fnde_maraba.xlsx")
ibge_regional=read_excel("C:/Users/Yasmin/Documents/NEPA-Tmm/regioes_geograficas_composicao_por_municipios_2017_20180911.xlsx")
geocod_uf=read_excel("C:/Users/Yasmin/Documents/NEPA-Tmm/geocod_mar2022.xlsx")

# Dados Marabá - cruzar dados fnde com divisão regional do BR 

#1. Cruzamento: eexgeocod x CD_GEOCODI
colnames(ibge_regional)
colnames(fnde_maraba)
ibge_regional=ibge_regional%>%rename("eexgeocod"="CD_GEOCODI")

fnde_maraba_001=merge(fnde_maraba,ibge_regional, by="eexgeocod")
colnames(fnde_maraba_001)
fnde_maraba_001=fnde_maraba_001%>%rename("eex_cod_rgi"="cod_rgi", "eex_nome_rgi"="nome_rgi", "eex_cod_rgint"="cod_rgint", "eex_nome_rgint"="nome_rgint")
View(fnde_maraba_001)

is.na(fnde_maraba_001$eex_cod_rgi)

#2. Cruzamento: fornecgeocod x CD_GEOCODI
colnames(ibge_regional)
colnames(fnde_maraba)
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
colnames(fnde_maraba)
geocod_uf=geocod_uf%>%select(1,2)
colnames(geocod_uf)=c("eexgeocod","eex_uf")
colnames(geocod_uf)

fnde_maraba_004=merge(fnde_maraba_003, geocod_uf, by="eexgeocod")
colnames(fnde_maraba_004)

#4. Cruzamento: fornecgeocod x CODMunic
colnames(geocod_uf)=c("fornecgeocod", "fornec_uf")
colnames(geocod_uf)

fnde_maraba_005=merge(fnde_maraba_004, geocod_uf, by="fornecgeocod")
colnames(fnde_maraba_005)

## Selecionar fornecedores de Marabá!!!
### fornecgeocod=1504208
unique(fnde_maraba_005$fornecgeocod)
unique(fnde_maraba_005$eexgeocod)
fnde_maraba_005=fnde_maraba_005%>%filter(fornecgeocod==1504208)
unique(fnde_maraba_005$fornecgeocod)

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

## Oferta para o próprio município:
oferta_maraba_maraba = fnde_maraba_005%>%filter(fornecgeocod==eexgeocod)


### 2013
nrow(fnde_maraba_13)
oferta_m_m_13=oferta_maraba_maraba%>%filter(ano==2013)
nrow(oferta_m_m_13)
percent_oferta_m_m_13=((sum(oferta_m_m_13$sum))/(sum(fnde_maraba_13$sum)))*100
percent_oferta_m_m_13

### 2014
nrow(fnde_maraba_14)
nrow(oferta_m_m_14)
oferta_m_m_14=oferta_maraba_maraba%>%filter(ano==2014)
percent_oferta_m_m_14=((sum(oferta_m_m_14$sum))/(sum(fnde_maraba_14$sum)))*100
percent_oferta_m_m_14

### 2015
nrow(fnde_maraba_15)
oferta_m_m_15=oferta_maraba_maraba%>%filter(ano==2015)
nrow(oferta_m_m_15)
percent_oferta_m_m_15=((sum(oferta_m_m_15$sum))/(sum(fnde_maraba_15$sum)))*100
percent_oferta_m_m_15

### 2016
oferta_m_m_16=oferta_maraba_maraba%>%filter(ano==2016)
percent_oferta_m_m_16=((sum(oferta_m_m_16$sum))/(sum(fnde_maraba_16$sum)))*100
percent_oferta_m_m_16
nrow(oferta_m_m_16)
nrow(fnde_maraba_16)

### 2017
oferta_m_m_17=oferta_maraba_maraba%>%filter(ano==2017)
percent_oferta_m_m_17=((sum(oferta_m_m_17$sum))/(sum(fnde_maraba_17$sum)))*100
percent_oferta_m_m_17

### 2018
oferta_m_m_18=oferta_maraba_maraba%>%filter(ano==2018)
percent_oferta_m_m_18=((sum(oferta_m_m_18$sum))/(sum(fnde_maraba_18$sum)))*100
percent_oferta_m_m_18

### 2019
oferta_m_m_19=oferta_maraba_maraba%>%filter(ano==2019)
percent_oferta_m_m_19=((sum(oferta_m_m_19$sum))/(sum(fnde_maraba_19$sum)))*100
percent_oferta_m_m_19

### 2020
oferta_m_m_20=oferta_maraba_maraba%>%filter(ano==2020)
percent_oferta_m_m_20=((sum(oferta_m_m_20$sum))/(sum(fnde_maraba_20$sum)))*100
percent_oferta_m_m_20

#####
# Dataset
#####
vetor_percent_oferta_m_m=c(percent_oferta_m_m_13,percent_oferta_m_m_14,percent_oferta_m_m_15, percent_oferta_m_m_16, percent_oferta_m_m_17, percent_oferta_m_m_18, percent_oferta_m_m_19, percent_oferta_m_m_20)
matriz_percent_oferta_maraba_maraba=matrix(data=vetor_percent_oferta_m_m, byrow = TRUE)
matriz_percent_oferta_maraba_maraba

vector_anos=c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
matriz_anos=matrix(data=vector_anos, byrow = TRUE)

data_percent_oferta_maraba_maraba=cbind(matriz_anos,matriz_percent_oferta_maraba_maraba)
View(data_percent_oferta_maraba_maraba)
colnames(data_percent_oferta_maraba_maraba)=c("Ano","Percentual adquirido do Mun. de Marabá")

## Demanda da região geográfica imediata
oferta_maraba_rgi=fnde_maraba_005%>%filter(fornec_cod_rgi==eex_cod_rgi)
nrow(oferta_maraba_rgi)
nrow(fnde_maraba_005)

### 2013
oferta_m_rgi_13=oferta_maraba_rgi%>%filter(ano==2013)
percent_oferta_m_rgi_13=((sum(oferta_m_rgi_13$sum))/(sum(fnde_maraba_13$sum)))*100
percent_oferta_m_rgi_13

### 2014
oferta_m_rgi_14=oferta_maraba_rgi%>%filter(ano==2014)
percent_oferta_m_rgi_14=((sum(oferta_m_rgi_14$sum))/(sum(fnde_maraba_14$sum)))*100
percent_oferta_m_rgi_14

### 2015
oferta_m_rgi_15=oferta_maraba_rgi%>%filter(ano==2015)
percent_oferta_m_rgi_15=((sum(oferta_m_rgi_15$sum))/(sum(fnde_maraba_15$sum)))*100
percent_oferta_m_rgi_15

### 2016
oferta_m_rgi_16=oferta_maraba_rgi%>%filter(ano==2016)
percent_oferta_m_rgi_16=((sum(oferta_m_rgi_16$sum))/(sum(fnde_maraba_16$sum)))*100
percent_oferta_m_rgi_16

### 2017
oferta_m_rgi_17=oferta_maraba_rgi%>%filter(ano==2017)
percent_oferta_m_rgi_17=((sum(oferta_m_rgi_17$sum))/(sum(fnde_maraba_17$sum)))*100
percent_oferta_m_rgi_17

### 2018
oferta_m_rgi_18=oferta_maraba_rgi%>%filter(ano==2018)
percent_oferta_m_rgi_18=((sum(oferta_m_rgi_18$sum))/(sum(fnde_maraba_18$sum)))*100
percent_oferta_m_rgi_18

### 2019
oferta_m_rgi_19=oferta_maraba_rgi%>%filter(ano==2019)
percent_oferta_m_rgi_19=((sum(oferta_m_rgi_19$sum))/(sum(fnde_maraba_19$sum)))*100
percent_oferta_m_rgi_19

### 2020
oferta_m_rgi_20=oferta_maraba_rgi%>%filter(ano==2020)
percent_oferta_m_rgi_20=((sum(oferta_m_rgi_20$sum))/(sum(fnde_maraba_20$sum)))*100
percent_oferta_m_rgi_20

#####
# Dataset
#####
vetor_percent_oferta_m_rgi=c(percent_oferta_m_rgi_13,percent_oferta_m_rgi_14,percent_oferta_m_rgi_15, percent_oferta_m_rgi_16, percent_oferta_m_rgi_17, percent_oferta_m_rgi_18, percent_oferta_m_rgi_19, percent_oferta_m_rgi_20)
matriz_percent_maraba_rgi=matrix(data=vetor_percent_oferta_m_rgi, byrow = TRUE)
matriz_percent_maraba_rgi

vector_anos=c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
matriz_anos=matrix(data=vector_anos, byrow = TRUE)

data_percent_maraba_rgi=cbind(matriz_anos,matriz_percent_maraba_rgi)
View(data_percent_maraba_rgi)
colnames(data_percent_maraba_rgi)=c("Ano","Percentual adquirido da RGI Marabá")

## Demanda da região geográfica intermediária
oferta_maraba_rgint=fnde_maraba_005%>%filter(fornec_cod_rgint==eex_cod_rgint)
nrow(oferta_maraba_rgint)
nrow(fnde_maraba_005)
## 100% em todos os anos!


# SALVANDOOO
data_percent_oferta_maraba_maraba=as.data.frame(data_percent_oferta_maraba_maraba)
write_xlsx(data_percent_oferta_maraba_maraba, 'percent_oferta_maraba_maraba.xlsx')
data_percent_maraba_rgi=as.data.frame(data_percent_maraba_rgi)
write_xlsx(data_percent_maraba_rgi, 'percent_oferta_maraba_rgi.xlsx')




## Quais alimentos são ofertados para a prefeitura de Maraba?
#vendas locais:
colnames(oferta_maraba_maraba)
oferta_maraba_maraba$item=tolower(oferta_maraba_maraba$item)
oferta_maraba_maraba$item= stringi::stri_trans_general(str = oferta_maraba_maraba$item, id = "Latin-ASCII")
oferta_maraba_maraba$item= stringr::str_replace_all(oferta_maraba_maraba$item, "(?![_])[[:punct:]]", "")
oferta_maraba_maraba_item=oferta_maraba_maraba%>%group_by(item)%>%summarise(sum(sum))
View(oferta_maraba_maraba_item)
colnames(oferta_maraba_maraba_item)

# fruticultura
oferta_m_m_fruta=oferta_maraba_maraba_item[c(1,11,12,13,39,44,45,50,51,52,63,64),]
percent_mm_fruta=((sum(oferta_m_m_fruta$`sum(sum)`))/(sum(oferta_maraba_maraba_item$`sum(sum)`)))*100
percent_mm_fruta  

# hortaliças
oferta_m_m_hort=oferta_maraba_maraba_item[c(3,4,5,6,8,9,10,14,15,18,19,20,21,22,23,24,25,26,32,33,34,35,36,37,38,42,43,46,48,49,53,56),]
percent_mm_hort=((sum(oferta_m_m_hort$`sum(sum)`))/(sum(oferta_maraba_maraba_item$`sum(sum)`)))*100
percent_mm_hort

# conserva de fruta
oferta_m_m_cons=oferta_maraba_maraba_item[c(2,7,16,17,27,40,47,55,57,58,59,60,61,62),]
percent_mm_cons=((sum(oferta_m_m_cons$`sum(sum)`))/(sum(oferta_maraba_maraba_item$`sum(sum)`)))*100
percent_mm_cons

# derivados de trigo
oferta_m_m_dertrig=oferta_maraba_maraba_item[c(28,29,30,31,41),]
percent_mm_dertrig=((sum(oferta_m_m_dertrig$`sum(sum)`))/(sum(oferta_maraba_maraba_item$`sum(sum)`)))*100
percent_mm_dertrig

# aves e ovo (ovo)
oferta_m_m_ovo=oferta_maraba_maraba_item[c(54),]
percent_mm_ovo=((sum(oferta_m_m_ovo$`sum(sum)`))/(sum(oferta_maraba_maraba_item$`sum(sum)`)))*100
percent_mm_ovo

# vendas para outras cidades:
colnames(fnde_maraba_005)
oferta_maraba_outras = fnde_maraba_005%>%filter(fornecgeocod!=eexgeocod)
nrow(fnde_maraba_005)
nrow(oferta_maraba_maraba)
nrow(oferta_maraba_outras)

oferta_maraba_outras$item=tolower(oferta_maraba_outras$item)
oferta_maraba_outras$item= stringi::stri_trans_general(str = oferta_maraba_outras$item, id = "Latin-ASCII")
oferta_maraba_outras$item= stringr::str_replace_all(oferta_maraba_outras$item, "(?![_])[[:punct:]]", "")
oferta_maraba_outras_item=oferta_maraba_outras%>%group_by(item)%>%summarise(sum(sum))
View(oferta_maraba_outras_item)

install.packages("writexl")
library(writexl)
write_xlsx(oferta_maraba_maraba_item, 'alimentos_oferta_maraba_maraba.xlsx')
write_xlsx(oferta_maraba_outras_item, 'alimentos_oferta_maraba_outras.xlsx')

# fruticultura
oferta_m_outras_fruta=oferta_maraba_outras_item[c(6,7),]
percent_moutras_fruta=((sum(oferta_m_outras_fruta$`sum(sum)`))/(sum(oferta_maraba_outras_item$`sum(sum)`)))*100
percent_moutras_fruta

# hortaliças
oferta_m_outras_hort=oferta_maraba_outras_item[c(2,3,5,10,12,14,16,17,18),]
percent_moutras_hort=((sum(oferta_m_outras_hort$`sum(sum)`))/(sum(oferta_maraba_outras_item$`sum(sum)`)))*100
percent_moutras_hort

# conservas de frutas
oferta_m_outras_cons=oferta_maraba_outras_item[c(1,4,8,9,13,15,19,20,22,23,24,25,26,27,28,29),]
percent_moutras_cons=((sum(oferta_m_outras_cons$`sum(sum)`))/(sum(oferta_maraba_outras_item$`sum(sum)`)))*100
percent_moutras_cons

# derivados de trigo
oferta_m_outras_dertrig=oferta_maraba_outras_item[c(21),]
percent_moutras_dertrig=((sum(oferta_m_outras_dertrig$`sum(sum)`))/(sum(oferta_maraba_outras_item$`sum(sum)`)))*100
percent_moutras_dertrig