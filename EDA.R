install.packages("janitor")
install.packages("gganimate")
install.packages("rcartocolor")

library(tidyverse)
library(lubridate)
library(janitor)
library(gganimate)
library(rcartocolor)

sg <- read.csv("sgdc_anom.csv", sep = ",")
str(sg)

length(unique(sg[year(sg$data_solic) == 2004, ]$cod_proc))

#Calcula a quantidade de novos processos abertos por ano

nvprocessos = data.frame(Ano = 2004,
                         Novos_Processo = length(unique(sg[year(sg$data_solic) == 2004, ]$cod_proc)))
for (i in 2005:2019) {
  univ <- length(unique(sg[year(sg$data_solic) >= 2004 & year(sg$data_solic) <= i, ]$cod_proc))
  print(univ)
  novosproc = univ - length(unique(sg[year(sg$data_solic) >= 2004 & year(sg$data_solic) <= i-1, ]$cod_proc))
  print(novosproc)
  nvprocessos <- rbind(data.frame(Ano = i, Novos_Processo = novosproc), nvprocessos)
}

nvprocessos %>% ggplot(aes(x = Ano, y = Novos_Processo)) +geom_line() 

abert_ano <- count(year(sg$data_solic))
nvprocessos <- nvprocessos %>% arrange(nvprocessos, Ano)
nvprocessos <- nvprocessos %>% mutate(sol_totais = abert_ano$freq, proporcao = (Novos_Processo / sol_totais)*100 )
nvprocessos %>% ggplot(aes(fill = proporcao, x = Ano, y = sol_totais)) +  geom_bar(position = "fill", stat = "Identity") + geom_bar(stat = "Identity")
