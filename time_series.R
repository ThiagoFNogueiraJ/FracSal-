require(ggplot2)
library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(lubridate)
library(plyr)



# Carrega o dataset em .csv
sol1418 <- read.csv("solicitações14a18.csv")
sol1418$Dt.Abertura <- as_date(sol1418$Dt.Abertura)

# Facet by two variables: vs and am.
# Rows are vs and columns are am
p3 <-  ggplot(data = sol1418, aes(Prefeitura.Bairro, Tipo.de.Ocorrência)) + geom_histogram()
    p3 + facet_grid( Prefeitura.Bairro~ Tipo.de.Ocorrência)
p3
#Filtra o dataset por ano
s2014 = filter(sol1418, year(Dt.Abertura) == 2018)
#Converte  as solicitações em uma série temporal de solicitações por dia
ts2014 <- as.data.frame(count(s2014$Dt.Abertura))
#completa a série com as datas faltantes
ts2014 <- ts2014 %>% 
  complete(x = seq(ymd("2018-01-01"), ymd("2018-12-31"), by = "1 day")) 
  ts2014[is.na(ts2014)] <- 0
                              
cont <- count(ts2014$freq)
names(cont)[1] <- ('obs')
p2 <- ggplot(cont, aes(x = obs, y = freq)) + geom_point() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() 
p2 + annotation_logticks(sides="trbl")  



