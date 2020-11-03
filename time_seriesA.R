#--------------#
require(scales)
library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(lubridate)
library(plyr)

#Lê o arquivo csv
sgdc <- read.csv("sgdc_anom.csv", sep = ",")

#Separa a data_hora
sgdc <- sgdc %>%  separate(data_solic, 
                           into = c("data_proc", "hora_proc"),
                           sep = " ",
                           remove = FALSE)

unique(sgdc$ocorr_solic)

#conmta as observações pela data e organiza num dataframe
serie_temp <- as.data.frame(count(sgdc[sgdc$ocorr_solic == "DESLIZAMENTO DE TERRA" & sgdc$ocorr_vist == "DESLIZAMENTO DE TERRA",], "data_proc"))

#converte data_proc de char para date
serie_temp$data_proc <- as_date(serie_temp$data_proc)
serie_temp

#completa a série com 0
serie_temp <- serie_temp %>% 
              complete(data_proc = seq(ymd("2004-07-01"), ymd("2019-12-31"), by = "1 day"))
serie_temp[is.na(serie_temp)] <- 0

filter(serie_temp, data_proc >= ymd("2005-01-01") & data_proc <= ymd("2019-12-31") ) %>%  ggplot(aes(x = data_proc, y = freq)) + geom_line()

#Cria a série temporal que será exportada
serie_export <- filter(serie_temp, data_proc >= ymd("2005-01-01") & data_proc <= ymd("2019-12-31"))

#Substitui as datas pela sequência de números
serie_export <- serie_export  %>%  mutate(dias = seq(1, nrow(serie_export)))

#Reordena as colunas do dataset 
serie_export <- serie_export[, c(3,2,1)]

#Remove a coluna de datas
serie_export <- subset(serie_export, select = -c(data_proc))

serie_export  %>%  ggplot(aes(x = dias, y = freq )) +geom_point()

#exporta o arquivo .tsv
write.table(serie_export  , file='socDDT.tsv', quote=FALSE, sep='\t', row.names = FALSE) 

