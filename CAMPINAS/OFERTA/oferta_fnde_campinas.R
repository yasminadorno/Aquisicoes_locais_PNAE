# Vendas locais - Campinas
# Objetivo 2: investigar a oferta de alimentos da agricultura familiar de Campinas para o PNAE
## Quais alimentos são vendidos pela AF Campinas
## Quantos % são ofertados para Campinas?
## Quais alimentos são ofertados para outras cidades? Estes alimentos são demandados pela prefeitura de Campinas?

library(dplyr)

#Dados:
fnde_campinas=read_excel("C:/Users/Yasmin/Documents/NEPA-TCC/fnde_campinas_v2.xlsx")
#ibge_regional=read_excel("C:/Users/Yasmin/Documents/NEPA-TCC/regioes_geograficas_composicao_por_cunicipios_2017_20180911.xlsx")
#geocod_uf=read_excel("C:/Users/Yasmin/Documents/NEPA-TCC/geocod_car2022.xlsx")

#1. Cruzamento: eexgeocod x CD_GEOCODI
colnames(fnde_campinas)
colnames(ibge_regional)
ibge_regional=ibge_regional%>%rename("geocodigo_eex"="geocodigo_fornec")

fnde_campinas_001=merge(fnde_campinas,ibge_regional, by="geocodigo_eex")
head(fnde_campinas_001)
colnames(fnde_campinas_001)
fnde_campinas_001=fnde_campinas_001%>%rename("eex_cod_rgi"="cod_rgi", "eex_nome_rgi"="nome_rgi", "eex_cod_rgint"="cod_rgint", "eex_nome_rgint"="nome_rgint")

is.na(fnde_campinas_001$eex_cod_rgi)

#2. Cruzamento: fornecgeocod x CD_GEOCODI
colnames(ibge_regional)
colnames(fnde_campinas)
ibge_regional=ibge_regional%>%rename("geocodigo_fornec"="geocodigo_eex")

fnde_campinas_002=merge(fnde_campinas_001, ibge_regional, by="geocodigo_fornec")
colnames(fnde_campinas_002)

fnde_campinas_002=fnde_campinas_002%>%rename("fornec_cod_rgi"="cod_rgi", "fornec_nome_rgi"="nome_rgi", "fornec_cod_rgint"="cod_rgint", "fornec_nome_rgint"="nome_rgint")
colnames(fnde_campinas_002)

fnde_campinas_003=fnde_campinas_002%>%select(1,2,4,6,7,11,14,16,17,18,19,21,22,23,24)
colnames(fnde_campinas_003)
View(fnde_campinas_003)

#3. Cruzamento: eexgeocod x CODMunic
colnames(geocod_uf)
geocod_uf=geocod_uf%>%select(1,2)
colnames(geocod_uf)=c("geocodigo_eex","eex_uf")
colnames(geocod_uf)

fnde_campinas_004=merge(fnde_campinas_003, geocod_uf, by="geocodigo_eex")
colnames(fnde_campinas_004)

#4. Cruzamento: fornecgeocod x CODMunic
colnames(geocod_uf)=c("geocodigo_fornec", "fornec_uf")
colnames(geocod_uf)

fnde_campinas_005=merge(fnde_campinas_004, geocod_uf, by="geocodigo_fornec")
colnames(fnde_campinas_005)

## Selecionar fornecedores de Campinas!!!
### fornecgeocod=3509502
fnde_campinas_005=fnde_campinas_005%>%filter(geocodigo_fornec==3509502)
unique(fnde_campinas_005$geocodigo_fornec)
unique(fnde_campinas_005$geocodigo_eex)
View(fnde_campinas_005)

#####
##Ajeitar as classes:
class(fnde_campinas_005$valor)
fnde_campinas_005$valor=as.numeric(fnde_campinas_005$valor)
######

fnde_campinas_13=fnde_campinas_005%>%filter(ano==2013)
fnde_campinas_14=fnde_campinas_005%>%filter(ano==2014)
fnde_campinas_15=fnde_campinas_005%>%filter(ano==2015)
fnde_campinas_16=fnde_campinas_005%>%filter(ano==2016)
fnde_campinas_17=fnde_campinas_005%>%filter(ano==2017)
fnde_campinas_18=fnde_campinas_005%>%filter(ano==2018)
fnde_campinas_19=fnde_campinas_005%>%filter(ano==2019)
fnde_campinas_20=fnde_campinas_005%>%filter(ano==2020)

## Oferta para o município de Campinas
oferta_campinas_campinas = fnde_campinas_005%>%filter(geocodigo_fornec==geocodigo_eex)
nrow(oferta_campinas_campinas)
nrow(fnde_campinas_005)
View(oferta_campinas_campinas)

oferta_c_c_13=oferta_campinas_campinas%>%filter(ano==2013)
oferta_c_c_14=oferta_campinas_campinas%>%filter(ano==2014)
oferta_c_c_15=oferta_campinas_campinas%>%filter(ano==2015)
oferta_c_c_16=oferta_campinas_campinas%>%filter(ano==2016)
oferta_c_c_17=oferta_campinas_campinas%>%filter(ano==2017)
oferta_c_c_18=oferta_campinas_campinas%>%filter(ano==2018)
oferta_c_c_19=oferta_campinas_campinas%>%filter(ano==2019)
oferta_c_c_20=oferta_campinas_campinas%>%filter(ano==2020)

### 2013
nrow(oferta_c_c_13)
percent_oferta_c_c_13=((sum(oferta_c_c_13$valor))/(sum(fnde_campinas_13$valor)))*100
percent_oferta_c_c_13

### 2014
nrow(oferta_c_c_14)
percent_oferta_c_c_14=((sum(oferta_c_c_14$valor))/(sum(fnde_campinas_14$valor)))*100
percent_oferta_c_c_14

### 2015
nrow(oferta_c_c_15)
percent_oferta_c_c_15=((sum(oferta_c_c_15$valor))/(sum(fnde_campinas_15$valor)))*100
percent_oferta_c_c_15

### 2016
percent_oferta_c_c_16=((sum(oferta_c_c_16$valor))/(sum(fnde_campinas_16$valor)))*100
percent_oferta_c_c_16

### 2017
percent_oferta_c_c_17=((sum(oferta_c_c_17$valor))/(sum(fnde_campinas_17$valor)))*100
percent_oferta_c_c_17

### 2018
percent_oferta_c_c_18=((sum(oferta_c_c_18$valor))/(sum(fnde_campinas_18$valor)))*100
percent_oferta_c_c_18

### 2019
percent_oferta_c_c_19=((sum(oferta_c_c_19$valor))/(sum(fnde_campinas_19$valor)))*100
percent_oferta_c_c_19

### 2020
percent_oferta_c_c_20=((sum(oferta_c_c_20$valor))/(sum(fnde_campinas_20$valor)))*100
percent_oferta_c_c_20

#####
# Dataset
#####
vetor_percent_oferta_c_c=c(percent_oferta_c_c_13,percent_oferta_c_c_14,percent_oferta_c_c_15, percent_oferta_c_c_16, percent_oferta_c_c_17, percent_oferta_c_c_18, percent_oferta_c_c_19, percent_oferta_c_c_20)
matriz_percent_campinas_campinas=matrix(data=vetor_percent_oferta_c_c, byrow = TRUE)
matriz_percent_campinas_campinas

vector_anos=c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
matriz_anos=matrix(data=vector_anos, byrow = TRUE)

data_percent_campinas_campinas=cbind(matriz_anos,matriz_percent_campinas_campinas)
View(data_percent_campinas_campinas)
colnames(data_percent_campinas_campinas)=c("Ano","Percentual adquirido do Mun. de Campinas")

## Demanda da região geográfica imediata
oferta_campinas_rgi=fnde_campinas_005%>%filter(fornec_cod_rgi==eex_cod_rgi)
View(oferta_campinas_rgi)

fnde_c_rgi_13=oferta_campinas_rgi%>%filter(ano==2013)
fnde_c_rgi_14=oferta_campinas_rgi%>%filter(ano==2014)
fnde_c_rgi_15=oferta_campinas_rgi%>%filter(ano==2015)
fnde_c_rgi_16=oferta_campinas_rgi%>%filter(ano==2016)
fnde_c_rgi_17=oferta_campinas_rgi%>%filter(ano==2017)
fnde_c_rgi_18=oferta_campinas_rgi%>%filter(ano==2018)
fnde_c_rgi_19=oferta_campinas_rgi%>%filter(ano==2019)
fnde_c_rgi_20=oferta_campinas_rgi%>%filter(ano==2020)

### 2013
percent_oferta_c_rgi_13=((sum(fnde_c_rgi_13$valor))/(sum(fnde_campinas_13$valor)))*100
percent_oferta_c_rgi_13

### 2014
percent_oferta_c_rgi_14=((sum(fnde_c_rgi_14$valor))/(sum(fnde_campinas_14$valor)))*100
percent_oferta_c_rgi_14

### 2015
percent_oferta_c_rgi_15=((sum(fnde_c_rgi_15$valor))/(sum(fnde_campinas_15$valor)))*100
percent_oferta_c_rgi_15

### 2016
percent_oferta_c_rgi_16=((sum(fnde_c_rgi_16$valor))/(sum(fnde_campinas_16$valor)))*100
percent_oferta_c_rgi_16

### 2017
percent_oferta_c_rgi_17=((sum(fnde_c_rgi_17$valor))/(sum(fnde_campinas_17$valor)))*100
percent_oferta_c_rgi_17

### 2018
percent_oferta_c_rgi_18=((sum(fnde_c_rgi_18$valor))/(sum(fnde_campinas_18$valor)))*100
percent_oferta_c_rgi_18

### 2019
percent_oferta_c_rgi_19=((sum(fnde_c_rgi_19$valor))/(sum(fnde_campinas_19$valor)))*100
percent_oferta_c_rgi_19

### 2020
percent_oferta_c_rgi_20=((sum(fnde_c_rgi_20$valor))/(sum(fnde_campinas_20$valor)))*100
percent_oferta_c_rgi_20

#####
# Dataset
#####
vetor_percent_oferta_c_rgi=c(percent_oferta_c_rgi_13,percent_oferta_c_rgi_14,percent_oferta_c_rgi_15, percent_oferta_c_rgi_16, percent_oferta_c_rgi_17, percent_oferta_c_rgi_18, percent_oferta_c_rgi_19, percent_oferta_c_rgi_20)
matriz_percent_campinas_rgi=matrix(data=vetor_percent_oferta_c_rgi, byrow = TRUE)
matriz_percent_campinas_rgi

data_percent_campinas_rgi=cbind(matriz_anos,matriz_percent_campinas_rgi)
View(data_percent_campinas_rgi)
colnames(data_percent_campinas_rgi)=c("Ano","Percentual adquirido da RGI Campinas")

## Demanda da região geográfica intermediária
oferta_campinas_rgint=fnde_campinas_005%>%filter(fornec_cod_rgint==eex_cod_rgint)
View(oferta_campinas_rgint)

fnde_c_rgint_13=oferta_campinas_rgint%>%filter(ano==2013)
fnde_c_rgint_14=oferta_campinas_rgint%>%filter(ano==2014)
fnde_c_rgint_15=oferta_campinas_rgint%>%filter(ano==2015)
fnde_c_rgint_16=oferta_campinas_rgint%>%filter(ano==2016)
fnde_c_rgint_17=oferta_campinas_rgint%>%filter(ano==2017)
fnde_c_rgint_18=oferta_campinas_rgint%>%filter(ano==2018)
fnde_c_rgint_19=oferta_campinas_rgint%>%filter(ano==2019)
fnde_c_rgint_20=oferta_campinas_rgint%>%filter(ano==2020)

### 2013
percent_oferta_c_rgint_13=((sum(fnde_c_rgint_13$valor))/(sum(fnde_campinas_13$valor)))*100
percent_oferta_c_rgint_13

### 2014
percent_oferta_c_rgint_14=((sum(fnde_c_rgint_14$valor))/(sum(fnde_campinas_14$valor)))*100
percent_oferta_c_rgint_14

### 2015
percent_oferta_c_rgint_15=((sum(fnde_c_rgint_15$valor))/(sum(fnde_campinas_15$valor)))*100
percent_oferta_c_rgint_15

### 2016
percent_oferta_c_rgint_16=((sum(fnde_c_rgint_16$valor))/(sum(fnde_campinas_16$valor)))*100
percent_oferta_c_rgint_16

### 2017
percent_c_rgint_17=((sum(fnde_c_rgint_17$valor))/(sum(fnde_campinas_17$valor)))*100
percent_c_rgint_17

### 2018
percent_c_rgint_18=((sum(fnde_c_rgint_18$valor))/(sum(fnde_campinas_18$valor)))*100
percent_c_rgint_18

### 2019
percent_c_rgint_19=((sum(fnde_c_rgint_19$valor))/(sum(fnde_campinas_19$valor)))*100
percent_c_rgint_19

### 2020
percent_c_rgint_20=((sum(fnde_c_rgint_20$valor))/(sum(fnde_campinas_20$valor)))*100
percent_c_rgint_20

##100! é ofertado para a região intermediária

# SALVANDOOOO
data_percent_campinas_campinas=as.data.frame(data_percent_campinas_campinas)
write_xlsx(data_percent_campinas_campinas , 'percent_oferta_campinas_campinas.xlsx')
data_percent_campinas_rgi=as.data.frame(data_percent_campinas_rgi)
write_xlsx(oferta_campinas_outras_item, 'percent_oferta_campinas_rgi.xlsx')

## Quais alimentos são ofertados para a prefeitura de Campinas?
#vendas locais:
colnames(oferta_campinas_campinas)
oferta_campinas_campinas$item=tolower(oferta_campinas_campinas$item)
oferta_campinas_campinas$item= stringi::stri_trans_general(str = oferta_campinas_campinas$item, id = "Latin-ASCII")
oferta_campinas_campinas$item= stringr::str_replace_all(oferta_campinas_campinas$item, "(?![_])[[:punct:]]", "")
oferta_campinas_campinas_item=oferta_campinas_campinas%>%group_by(item)%>%summarise(sum(valor))
View(oferta_campinas_campinas_item)
colnames(oferta_campinas_campinas_item)

# fruticultura
oferta_c_c_fruta=oferta_campinas_campinas_item[c(1,7,19,20,21,22),]
sum(oferta_c_c_fruta$`sum(valor)`)
percent_cc_fruta=((sum(oferta_c_c_fruta$`sum(valor)`))/(sum(oferta_campinas_campinas_item$`sum(valor)`)))*100
percent_cc_fruta  

# hortaliças
oferta_c_c_hort=oferta_campinas_campinas_item[c(2,3,4,5,6,8,9,10,11,12,13,14,15,16,17,23,24,25,26,27,28,29),]
percent_cc_hort=((sum(oferta_c_c_hort$`sum(valor)`))/(sum(oferta_campinas_campinas_item$`sum(valor)`)))*100
percent_cc_hort

# der. leite
oferta_c_c_leite=oferta_campinas_campinas_item[c(18),]
percent_cc_leite=((sum(oferta_c_c_leite$`sum(valor)`))/(sum(oferta_campinas_campinas_item$`sum(valor)`)))*100
percent_cc_leite

# vendas para outras cidades:
oferta_campinas_outras = fnde_campinas_005%>%filter(geocodigo_fornec!=geocodigo_eex)
nrow(fnde_campinas_005)
nrow(oferta_campinas_campinas)
nrow(oferta_campinas_outras)

oferta_campinas_outras$item=tolower(oferta_campinas_outras$item)
oferta_campinas_outras$item= stringi::stri_trans_general(str = oferta_campinas_outras$item, id = "Latin-ASCII")
oferta_campinas_outras$item= stringr::str_replace_all(oferta_campinas_outras$item, "(?![_])[[:punct:]]", "")
oferta_campinas_outras_item=oferta_campinas_outras%>%group_by(item)%>%summarise(sum(valor))
View(oferta_campinas_outras_item)

# fruticultura
oferta_c_outras_fruta=oferta_campinas_outras_item[c(1),]
percent_coutras_fruta=((sum(oferta_c_outras_fruta$`sum(valor)`))/(sum(oferta_campinas_outras_item$`sum(valor)`)))*100
percent_coutras_fruta

# der. leite
oferta_c_outras_leite=oferta_campinas_outras_item[c(2,3,4),]
percent_coutras_leite=((sum(oferta_c_outras_leite$`sum(valor)`))/(sum(oferta_campinas_outras_item$`sum(valor)`)))*100
percent_coutras_leite

# conservas frutas/legumes e sucos
oferta_c_outras_cons=oferta_campinas_outras_item[c(5),]
percent_coutras_cons=((sum(oferta_c_outras_cons$`sum(valor)`))/(sum(oferta_campinas_outras_item$`sum(valor)`)))*100
percent_coutras_cons

library(writexl)
write_xlsx(oferta_campinas_campinas_item, 'alimentos_oferta_campinas_campinas.xlsx')
write_xlsx(oferta_campinas_outras_item, 'alimentos_oferta_campinas_outras.xlsx')

