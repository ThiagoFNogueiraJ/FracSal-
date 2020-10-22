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

#conmta as observações pela data e organiza num dataframe
serie_temp <- as.data.frame(count(sgdc[sgdc$ocorr_solic == "AMEACA DE DESLIZAMENTO",], "data_proc"))

#converte data_proc de char para date
serie_temp$data_proc <- as_date(serie_temp$data_proc)
serie_temp

#completa a série com 0
serie_temp <- serie_temp %>% 
              complete(data_proc = seq(ymd("2004-07-01"), ymd("2019-12-31"), by = "1 day"))
serie_temp[is.na(serie_temp)] <- 0

filter(serie_temp, year(data_proc) >= 2010 & year(data_proc) <= 2019 ) %>%  ggplot(aes(x = data_proc, y = freq)) + geom_line()
 