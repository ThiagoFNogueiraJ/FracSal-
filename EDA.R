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
  #print(novosproc)
  nvprocessos <- rbind(data.frame(Ano = i, Novos_Processo = novosproc), nvprocessos)
}


abert_ano <- sg %>% group_by(year(data_solic)) %>%  
                    summarise(freq = n())
nvprocessos <- nvprocessos %>% arrange(nvprocessos, Ano)
nvprocessos <- nvprocessos %>% mutate(sol_totais = abert_ano$freq, proporcao = (Novos_Processo / sol_totais)*100 )

#Gráfico de proporção entre novos processos e revistorias
nvprocessos %>% mutate(revist = sol_totais - Novos_Processo) %>% 
                          gather(Tipo, valor, -c(Ano, sol_totais, proporcao)) %>% 
                          ggplot() +
                          geom_bar(aes(x = Ano, y= valor, fill = factor(Tipo, levels = c("revist", "Novos_Processo"))), stat = "identity", position = "fill") 
                          labs(x = "ano",
                               y = "Proporção de novos processos",
                               title = "")