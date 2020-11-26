#--------------#
require(scales)
library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(lubridate)
library(plyr)

#carrega o dataset
sgdc <- read.csv("sgdc_quatro.txt", sep = "\t", encoding = "UTF-8")
str(sgdc)
dim(sgdc)
head(filter(sgdc, desc_status =="PENDENTE"))
count(sgdc[sgdc$ocorr_solic == "DESLIZAMENTO DE TERRA",], "ocorr_solic")
#converte datas da solicitação e de vistoria em formato de datahora
sgdc$data_proc <- ymd_hms(sgdc$data_proc)
sgdc$data_vist <- ymd_hms(sgdc$data_vist)

#remove linhas duplicadas
sgdc <- distinct(sgdc)

#RRemove solicitações com status em aberto e status aberto em campo
sgdc <- sgdc[!(sgdc$desc_status == "ABERTO"),]
sgdc <- sgdc[!(sgdc$desc_status == "ABERTO EM CAMPO"),]
sgdc <- sgdc[!(sgdc$desc_status == "ÓRGÃO SETORIAL"),]
sgdc <- sgdc[!(sgdc$cod_logra == 0),]



#Separa data e hora em duas colunas
sgdc <- sgdc %>%  separate(data_proc, 
                   into = c("data_proc", "hora_proc"),
                   sep = " ",
                   remove = FALSE)

#Remoção de duplicatas 
sgdc <- sgdc %>%  distinct(cod_logra, data_proc, num_imov_solic, ocorr_solic, nome_solicitante_solic,  .keep_all = TRUE)

#exporta dataset identificado 

write.csv(sgdc,"sgdc_identificado.csv", row.names = FALSE)


#Dropa colunas e as reordena 
sgdc <- subset(sgdc, select = -c(nome_solicitante_solic, cod_cliente, nome_cliente, apelido_cliente, cpf_cliente))
sgdc <- sgdc %>% mutate(data_solic = ymd(data_proc) + hms(hora_proc))
sgdc <- subset(sgdc, select = -c(data_proc, hora_proc))
sgdc <- sgdc[, c(3, 17, 1,10 ,2, 11, 12, 8, 9, 4, 5, 6, 7, 13, 14, 15, 16)]
sgdc <-subset(sgdc, select = -c(desc_tipo_atend))

dim(sgdc)
#exoirta csv anonimizado
write.csv(sgdc,"sgdc_anom.csv", row.names = FALSE)

abert_ano <- count(year(sg$data_solic))
nv
