# Dados - Compras Locais do PNAE - Campinas

# Objetivo 1: alimentos ofertados pela agricultura familiar de Campinas para o PNAE
    ## Quais alimentos são vendidos para Campinas?
    ## Quantos % são ofertados para Campinas?
    ## Qual a % em relação à produção total (Censo)

# Objetivo 2: alimentos adquiridos pela pref. de Campinas para o PNAE
    ## Quais alimentos são demandados da AF Campinas?
    ## Quantos % são demandandados de Campinas? ## OK
    ## Quais alimentos são demandandos de outras cidades? Estes alimentos são produzidos por Campinas?

#Compras Campinas - cruzar dados fnde com divisão regional do BR 
#Objetivo: obter regiões geográficas imediatas e intermediárias

library(readxl)
library(dplyr)
library(ggplot2)

#Dados:
fnde_campinas_demanda=read_excel("C:/Users/Yasmin/Documents/NEPA-TCC/fnde_campinas_v2.xlsx")
ibge_regional=read_excel("C:/Users/Yasmin/Documents/NEPA-TCC/regioes_geograficas_composicao_por_municipios_2017_20180911.xlsx")
geocod_uf=read_excel("C:/Users/Yasmin/Documents/NEPA-TCC/geocod_mar2022.xlsx")

#1. Cruzamento: eexgeocod x CD_GEOCODI
colnames(fnde_campinas)
colnames(ibge_regional)
ibge_regional=ibge_regional%>%rename("geocodigo_eex"="geocodigo_fornec")

fnde_campinas_001_d=merge(fnde_campinas_demanda,ibge_regional, by="geocodigo_eex")
head(fnde_campinas_001)
colnames(fnde_campinas_001)
fnde_campinas_001_d=fnde_campinas_001_d%>%rename("eex_cod_rgi"="cod_rgi", "eex_nome_rgi"="nome_rgi", "eex_cod_rgint"="cod_rgint", "eex_nome_rgint"="nome_rgint")

is.na(fnde_campinas_001$eex_cod_rgi)

#2. Cruzamento: fornecgeocod x CD_GEOCODI
colnames(ibge_regional)
colnames(fnde_campinas)
ibge_regional=ibge_regional%>%rename("geocodigo_fornec"="geocodigo_eex")

fnde_campinas_002_d=merge(fnde_campinas_001_d, ibge_regional, by="geocodigo_fornec")
colnames(fnde_campinas_002)

fnde_campinas_002_d=fnde_campinas_002_d%>%rename("fornec_cod_rgi"="cod_rgi", "fornec_nome_rgi"="nome_rgi", "fornec_cod_rgint"="cod_rgint", "fornec_nome_rgint"="nome_rgint")
colnames(fnde_campinas_002)

fnde_campinas_003_d=fnde_campinas_002_d%>%select(1,2,4,6,7,11,14,16,17,18,19,21,22,23,24)


#3. Cruzamento: eexgeocod x CODMunic
colnames(geocod_uf)
geocod_uf=geocod_uf%>%select(1,2)
colnames(geocod_uf)=c("geocodigo_eex","eex_uf")
colnames(geocod_uf)

fnde_campinas_004_d=merge(fnde_campinas_003_d, geocod_uf, by="geocodigo_eex")
colnames(fnde_campinas_004)

#4. Cruzamento: fornecgeocod x CODMunic
colnames(geocod_uf)=c("geocodigo_fornec", "fornec_uf")
colnames(geocod_uf)

fnde_campinas_005_d=merge(fnde_campinas_004_d, geocod_uf, by="geocodigo_fornec")
colnames(fnde_campinas_005)

fnde_campinas_005_d=fnde_campinas_005_d%>%filter(geocodigo_eex==3509502)
unique(fnde_campinas_005_d$geocodigo_eex)
unique(fnde_campinas_005_d$geocodigo_fornec)

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
class(fnde_campinas_20$valor)


## Demanda do município de Campinas
fnde_campinas_campinas = fnde_campinas_005%>%filter(geocodigo_fornec==geocodigo_eex)
View(fnde_campinas_campinas)
View(fnde_campinas_005)

fnde_c_c_13=fnde_campinas_campinas%>%filter(ano==2013)
fnde_c_c_14=fnde_campinas_campinas%>%filter(ano==2014)
fnde_c_c_15=fnde_campinas_campinas%>%filter(ano==2015)
fnde_c_c_16=fnde_campinas_campinas%>%filter(ano==2016)
fnde_c_c_17=fnde_campinas_campinas%>%filter(ano==2017)
fnde_c_c_18=fnde_campinas_campinas%>%filter(ano==2018)
fnde_c_c_19=fnde_campinas_campinas%>%filter(ano==2019)
fnde_c_c_20=fnde_campinas_campinas%>%filter(ano==2020)

### 2013
percent_c_c_13=((sum(fnde_c_c_13$valor))/(sum(fnde_campinas_13$valor)))*100
percent_c_c_13

### 2014
percent_c_c_14=((sum(fnde_c_c_14$valor))/(sum(fnde_campinas_14$valor)))*100
percent_c_c_14

### 2015
percent_c_c_15=((sum(fnde_c_c_15$valor))/(sum(fnde_campinas_15$valor)))*100
percent_c_c_15

### 2016
percent_c_c_16=((sum(fnde_c_c_16$valor))/(sum(fnde_campinas_16$valor)))*100
percent_c_c_16

### 2017
percent_c_c_17=((sum(fnde_c_c_17$valor))/(sum(fnde_campinas_17$valor)))*100
percent_c_c_17

### 2018
percent_c_c_18=((sum(fnde_c_c_18$valor))/(sum(fnde_campinas_18$valor)))*100
percent_c_c_18

### 2019
percent_c_c_19=((sum(fnde_c_c_19$valor))/(sum(fnde_campinas_19$valor)))*100
percent_c_c_19

### 2020
percent_c_c_20=((sum(fnde_c_c_20$valor))/(sum(fnde_campinas_20$valor)))*100
percent_c_c_20

#####
# Dataset
#####
vetor_percent_c_c=c(percent_c_c_13,percent_c_c_14,percent_c_c_15, percent_c_c_16, percent_c_c_17, percent_c_c_18, percent_c_c_19, percent_c_c_20)
matriz_percent_campinas_campinas=matrix(data=vetor_percent_c_c, byrow = TRUE)
matriz_percent_campinas_campinas

vector_anos=c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
matriz_anos=matrix(data=vector_anos, byrow = TRUE)

data_percent_campinas_campinas=cbind(matriz_anos,matriz_percent_campinas_campinas)
View(data_percent_campinas_campinas)
colnames(data_percent_campinas_campinas)=c("Ano","Percentual adquirido do Mun. de Campinas")

## Demanda da região geográfica imediata
fnde_campinas_rgi=fnde_campinas_005%>%filter(fornec_cod_rgi==eex_cod_rgi)

fnde_c_rgi_13=fnde_campinas_rgi%>%filter(ano==2013)
fnde_c_rgi_14=fnde_campinas_rgi%>%filter(ano==2014)
fnde_c_rgi_15=fnde_campinas_rgi%>%filter(ano==2015)
fnde_c_rgi_16=fnde_campinas_rgi%>%filter(ano==2016)
fnde_c_rgi_17=fnde_campinas_rgi%>%filter(ano==2017)
fnde_c_rgi_18=fnde_campinas_rgi%>%filter(ano==2018)
fnde_c_rgi_19=fnde_campinas_rgi%>%filter(ano==2019)
fnde_c_rgi_20=fnde_campinas_rgi%>%filter(ano==2020)

### 2013
percent_c_rgi_13=((sum(fnde_c_rgi_13$valor))/(sum(fnde_campinas_13$valor)))*100
percent_c_rgi_13

### 2014
percent_c_rgi_14=((sum(fnde_c_rgi_14$valor))/(sum(fnde_campinas_14$valor)))*100
percent_c_rgi_14

### 2015
percent_c_rgi_15=((sum(fnde_c_rgi_15$valor))/(sum(fnde_campinas_15$valor)))*100
percent_c_rgi_15

### 2016
percent_c_rgi_16=((sum(fnde_c_rgi_16$valor))/(sum(fnde_campinas_16$valor)))*100
percent_c_rgi_16

### 2017
percent_c_rgi_17=((sum(fnde_c_rgi_17$valor))/(sum(fnde_campinas_17$valor)))*100
percent_c_rgi_17

### 2018
percent_c_rgi_18=((sum(fnde_c_rgi_18$valor))/(sum(fnde_campinas_18$valor)))*100
percent_c_rgi_18

### 2019
percent_c_rgi_19=((sum(fnde_c_rgi_19$valor))/(sum(fnde_campinas_19$valor)))*100
percent_c_rgi_19

### 2020
percent_c_rgi_20=((sum(fnde_c_rgi_20$valor))/(sum(fnde_campinas_20$valor)))*100
percent_c_rgi_20

#####
# Dataset
#####
vetor_percent_c_rgi=c(percent_c_rgi_13,percent_c_rgi_14,percent_c_rgi_15, percent_c_rgi_16, percent_c_rgi_17, percent_c_rgi_18, percent_c_rgi_19, percent_c_rgi_20)
matriz_percent_campinas_rgi=matrix(data=vetor_percent_c_rgi, byrow = TRUE)
matriz_percent_campinas_rgi

data_percent_campinas_rgi=cbind(matriz_anos,matriz_percent_campinas_rgi)
View(data_percent_campinas_rgi)
colnames(data_percent_campinas_rgi)=c("Ano","Percentual adquirido da RGI Campinas")

## Demanda da região geográfica intermediária
fnde_campinas_rgint=fnde_campinas_005%>%filter(fornec_cod_rgint==eex_cod_rgint)
View(fnde_campinas_rgint)

fnde_c_rgint_13=fnde_campinas_rgint%>%filter(ano==2013)
fnde_c_rgint_14=fnde_campinas_rgint%>%filter(ano==2014)
fnde_c_rgint_15=fnde_campinas_rgint%>%filter(ano==2015)
fnde_c_rgint_16=fnde_campinas_rgint%>%filter(ano==2016)
fnde_c_rgint_17=fnde_campinas_rgint%>%filter(ano==2017)
fnde_c_rgint_18=fnde_campinas_rgint%>%filter(ano==2018)
fnde_c_rgint_19=fnde_campinas_rgint%>%filter(ano==2019)
fnde_c_rgint_20=fnde_campinas_rgint%>%filter(ano==2020)

### 2013
percent_c_rgint_13=((sum(fnde_c_rgint_13$valor))/(sum(fnde_campinas_13$valor)))*100
percent_c_rgint_13

### 2014
percent_c_rgint_14=((sum(fnde_c_rgint_14$valor))/(sum(fnde_campinas_14$valor)))*100
percent_c_rgint_14

### 2015
percent_c_rgint_15=((sum(fnde_c_rgint_15$valor))/(sum(fnde_campinas_15$valor)))*100
percent_c_rgint_15

### 2016
percent_c_rgint_16=((sum(fnde_c_rgint_16$valor))/(sum(fnde_campinas_16$valor)))*100
percent_c_rgint_16

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

#####
# Dataset
#####
vetor_percent_c_rgint=c(percent_c_rgint_13,percent_c_rgint_14,percent_c_rgint_15, percent_c_rgint_16, percent_c_rgint_17, percent_c_rgint_18, percent_c_rgint_19, percent_c_rgint_20)
matriz_percent_campinas_rgint=matrix(data=vetor_percent_c_rgint, byrow = TRUE)

data_percent_campinas_rgint=cbind(matriz_anos,matriz_percent_caraba_rgint)
View(data_percent_campinas_rgint)
colnames(data_percent_campinas_rgint)=c("Ano","Percentual adquirido da RGINT Campinas")

## Demanda do Estado
colnames(fnde_campinas_005)
fnde_campinas_uf=fnde_campinas_005%>%filter(fornec_uf==eex_uf)

View(fnde_campinas_005)

fnde_c_uf_13=fnde_campinas_uf%>%filter(ano==2013)
fnde_c_uf_14=fnde_campinas_uf%>%filter(ano==2014)
fnde_c_uf_15=fnde_campinas_uf%>%filter(ano==2015)
fnde_c_uf_16=fnde_campinas_uf%>%filter(ano==2016)
fnde_c_uf_17=fnde_campinas_uf%>%filter(ano==2017)
fnde_c_uf_18=fnde_campinas_uf%>%filter(ano==2018)
fnde_c_uf_19=fnde_campinas_uf%>%filter(ano==2019)
fnde_c_uf_20=fnde_campinas_uf%>%filter(ano==2020)

### 2013
percent_c_uf_13=((sum(fnde_c_uf_13$valor))/(sum(fnde_campinas_13$valor)))*100
percent_c_uf_13


### 2014
percent_c_uf_14=((sum(fnde_c_uf_14$valor))/(sum(fnde_campinas_14$valor)))*100
percent_c_uf_14


### 2015
percent_c_uf_15=((sum(fnde_c_uf_15$valor))/(sum(fnde_campinas_15$valor)))*100
percent_c_uf_15

### 2016
percent_c_uf_16=((sum(fnde_c_uf_16$valor))/(sum(fnde_campinas_16$valor)))*100
percent_c_uf_16

### 2017
percent_c_uf_17=((sum(fnde_c_uf_17$valor))/(sum(fnde_campinas_17$valor)))*100
percent_c_uf_17

### 2018
percent_c_uf_18=((sum(fnde_c_uf_18$valor))/(sum(fnde_campinas_18$valor)))*100
percent_c_uf_18

### 2019
percent_c_uf_19=((sum(fnde_c_uf_19$valor))/(sum(fnde_campinas_19$valor)))*100
percent_c_uf_19

### 2020
percent_c_uf_20=((sum(fnde_c_uf_20$valor))/(sum(fnde_campinas_20$valor)))*100
percent_c_uf_20


vetor_percent_campinas_uf=c(percent_c_uf_13,percent_c_uf_14,percent_c_uf_15,percent_c_uf_16,percent_c_uf_17, percent_c_uf_18, percent_c_uf_19, percent_c_uf_20)
matriz_percent_campinas_uf=matrix(data=vetor_percent_campinas_uf, byrow = TRUE)
matriz_percent_campinas_uf

data_percent_campinas_uf=cbind(matriz_anos,matriz_percent_campinas_uf)
View(data_percent_campinas_uf)
colnames(data_percent_campinas_uf)=c("Ano","Percentual adquirido do Estado de SP")


## Quais alimentos são demandados da AF Campinas?
colnames(fnde_campinas_005)

#compras locais:
nrow(fnde_campinas_005)
nrow(fnde_campinas_campinas)
class(fnde_campinas_campinas$valor)
fnde_campinas_campinas$valor=as.numeric(fnde_campinas_campinas$valor)
fnde_campinas_campinas_item=fnde_campinas_campinas%>%group_by(item)%>%summarise(sum(valor))
View(fnde_campinas_campinas_item)

fnde_c_c_hortalicas=fnde_campinas_campinas_item%>%filter(item=="abobora","abobrinha","acelga","alface","almeirao","batata doce","batata inglesa","berinjela", "beterraba", "cebola","cebolinha","cenoura","chuchu","couve","espinafre","pepino","pimentao verde","repolho","salsa","tomate para molho","tomate salada","vagem")
fnde_c_c_hortalicas=fnde_campinas_campinas_item[c(2,3,4,5,6,8,9,10,11,12,13,14,15,16,17,23,24,25,26,27,28,29),]
fnde_c_c_hortalicas
sum(fnde_c_c_hortalicas$`sum(valor)`)

sum(fnde_campinas_campinas_item$`sum(valor)`)

#compras de outras cidades:
fnde_campinas_outras = fnde_campinas_005%>%filter(geocodigo_fornec!=geocodigo_eex)
nrow(fnde_campinas_005)
nrow(fnde_campinas_campinas)
nrow(fnde_campinas_outras)
View(fnde_campinas_outras)
class(fnde_campinas_outras$valor)
fnde_campinas_outras$valor=as.numeric(fnde_campinas_outras$valor)

fnde_campinas_outras_item=fnde_campinas_outras%>%group_by(item)%>%summarise(sum(valor))
View(fnde_campinas_outras_item)

fnde_campinas_outras_hortalicas=fnde_campinas_outras_item[c(5,6,7,8,9,10,11,12,13,14,32,33,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,54,55,77,83,85,86,88,89,91,92,94,95,96,110,111,112),]
View(fnde_campinas_outras_hortalicas)
fnde_campinas_outras_hortalicas=fnde_campinas_outras_hortalicas%>%na.omit(fnde_campinas_outras_hortalicas)
percent_outras_hortalicas=((sum(fnde_campinas_outras_hortalicas$`sum(valor)`))/(sum(fnde_campinas_outras_item$`sum(valor)`)))
percent_outras_hortalicas

fnde_outras_lat=fnde_campinas_outras_item[c(34,35,58,59,60,61,65,66,67,68,69,70,90,93),]
percent_outras_lat=((sum(fnde_outras_lat$`sum(valor)`))/(sum(fnde_campinas_outras_item$`sum(valor)`)))
percent_outras_lat

fnde_outras_frutas=fnde_campinas_outras_item[c(1,2,3,4,25,26,27,28,29,30,31,39,56,57,62,63,64,71,72,75,76,78,79,80,81,84,87,109),]
percent_outras_frutas=((sum(fnde_outras_frutas$`sum(valor)`))/(sum(fnde_campinas_outras_item$`sum(valor)`)))
percent_outras_frutas

fnde_outras_cereais=fnde_campinas_outras_item[c(15,16,17,18,19,20,21,22,23,24),]
percent_outras_cereais=((sum(fnde_outras_cereais$`sum(valor)`))/(sum(fnde_campinas_outras_item$`sum(valor)`)))
percent_outras_cereais

fnde_outras_conservas=fnde_campinas_outras_item[c(97,98,99,100,101,102,103,104,105,106,107,108),]
percent_outras_conservas=((sum(fnde_outras_conservas$`sum(valor)`))/(sum(fnde_campinas_outras_item$`sum(valor)`)))
percent_outras_conservas

fnde_outras_dertrig=fnde_campinas_outras_item[c(53,73,74,82),]
percent_outras_dertrig=((sum(fnde_outras_dertrig$`sum(valor)`))/(sum(fnde_campinas_outras_item$`sum(valor)`)))
percent_outras_dertrig

sum(percent_outras_cereais,percent_outras_conservas,percent_outras_dertrig, percent_outras_frutas,percent_outras_hortalicas, percent_outras_lat)

nrow(fnde_outras_cereais)
nrow(fnde_outras_conservas)
nrow(fnde_outras_dertrig)
nrow(fnde_outras_frutas)
nrow(fnde_outras_lat)
nrow(fnde_campinas_outras_hortalicas)
nrow(fnde_campinas_outras_item)

library(writexl)
write_xlsx(fnde_campinas_campinas_item, 'alimentos_demanda_campinas_campinas.xlsx')
write_xlsx(fnde_campinas_outras_item, 'alimentos_demanda_campinas_outras.xlsx')

####### Rankear cidades com maiores vendas ao PNAE Campinas no período
class(fnde_campinas_005_d$valor)
fnde_campinas_005_d$valor=as.numeric(fnde_campinas_005_d$valor)
colnames(fnde_campinas_005_d)

## CIDADES RANKEADAS
fnde_campinas_d_city=fnde_campinas_005_d %>% group_by(fornmunicipio_uf) %>% summarise(sum(valor))
View(fnde_campinas_d_city)
fnde_campinas_d_city_ordered=fnde_campinas_d_city%>%arrange(desc(fnde_campinas_d_city$`sum(valor)`))
View(fnde_campinas_d_city_ordered)

## UF RANKEADO
fnde_campinas_d_uf=fnde_campinas_005_d %>% group_by(fornec_uf) %>% summarise(sum(valor))
fnde_campinas_d_uf_ordered=fnde_campinas_d_uf%>%arrange(desc(fnde_campinas_d_uf$`sum(valor)`))
View(fnde_campinas_d_uf_ordered)

## RGINT RANKEADO
fnde_campinas_d_rgint=fnde_campinas_005_d %>% group_by(fornec_nome_rgint) %>% summarise(sum(valor))
fnde_campinas_d_rgint_ordered=fnde_campinas_d_rgint%>%arrange(desc(fnde_campinas_d_rgint$`sum(valor)`))
View(fnde_campinas_d_rgint_ordered)

## RGI RANKEADO
fnde_campinas_d_rgi=fnde_campinas_005_d %>% group_by(fornec_nome_rgi) %>% summarise(sum(valor))
fnde_campinas_d_rgi_ordered=fnde_campinas_d_rgi%>%arrange(desc(fnde_campinas_d_rgi$`sum(valor)`))
View(fnde_campinas_d_rgi_ordered)

### Gráfico de Barras uf_ordered
demanda_uf=ggplot(fnde_campinas_d_uf_ordered,aes(x= fornec_uf, y=`sum(valor)`))+
  geom_bar(stat = "identity")+
  scale_y_continuous(limits = c(0,250000000), breaks = seq(0,250000000,25000000)) +
  theme_classic(base_size = 18)+
  xlab("Estado") + 
  ylab("Valor adquirido (R$)") +
demanda_uf

######
rotulo=c("Estado", "Valor adquirido (R$)")
par(mgp=c(1,1,0))+
  barplot(fnde_campinas_d_uf_ordered$`sum(valor)`, main="Aquisições de Alimentos pela Prefeitura de Campinas", xlab=rotulo[1], ylab=rotulo[2], names.arg = fnde_campinas_d_uf_ordered$fornec_uf, ylim=c(0, 250000000), cex.names = 0.8, xaxs = "i", col = 43)+
  grid(nx=NA, ny=NULL) +
  barplot(fnde_campinas_d_uf_ordered$`sum(valor)`, main="Aquisições de Alimentos pela Prefeitura de Campinas", xlab=rotulo[1], ylab=rotulo[2], names.arg = fnde_campinas_d_uf_ordered$fornec_uf, ylim=c(0, 250000000), cex.names = 0.8, xaxs = "i", col = 43, add = TRUE)


#####
## Dado (dividir por 100 mil)
soma=fnde_campinas_d_uf_ordered$`sum(valor)`
somanova=soma/100000
somanova

fnde_campinas_d_uf_ordered["valor"]=somanova
View(fnde_campinas_d_uf_ordered)

uf_base=fnde_campinas_d_uf_ordered%>%select(1,3)
View(uf_base)

## plotar gráfico
barplot(height=uf_base$valor, names=uf_base$fornec_uf, 
        col=rgb(0.2,0.6,0.1,0.6),
        xlab="Estado", 
        ylab="Valor adquirido (R$ 100 mil)", 
        main="Aquisições de Alimentos pela Prefeitura de Campinas por UF", 
        cex.axis = 0.7,  
        cex.names = 0.6,
        args.legend = list("bottom", bty="n", cex = 0.7),
        xpd=FALSE,
        cex.main = 0.9,
        mgp = c(1, 0.2, 0),
        ylim=c(0,2500),
        width=0.3
)

View(grafico)
